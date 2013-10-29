-module(node).
-export([start/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datastructures.hrl").


start(State) ->
  register_node(State#state.name, self()),
  loop(NewState).

loop(State) ->
  log("~p", [State]),

  receive
    {initiate,Level,FragName,NodeState,Edge} ->
      NewState = handle_initiate_message(State, Level, FragName, NodeState, Edge),
      loop(NewState);
    {test,Level,FragName,Edge} ->
      NewState = handle_test_message(State, Level, FragName, Edge),
      loop(NewState);
    {accept,Edge} ->
      NewState = handle_accept_message(State, Edge),
      loop(NewState);
    {reject,Edge} ->
      NewState = handle_reject_message(State, Edge),
      loop(NewState);
    {report,Weight,Edge} ->
      NewState = handle_report_message(State, Weight, Edge),
      loop(NewState);
    {changeroot,_} ->
      NewState = handle_changeroot_mesage(State),
      loop(NewState);
    {connect,Level,Edge} ->
      NewState = handle_connect_message(State, Level, Edge),
      loop(NewState)
  end.

get_target_pid(Edge) ->
  global:whereis_name(Edge#edge.node_2#node.name).

edge_to_tuple(Edge) ->
  { Edge#edge.weight, Edge#edge.node_1#node.name, Edge#edge.node_2#node.name }.

log(Format, Data) ->
  io:format("node: " ++ Format ++ "~n", Data).

register_node(NodeName, Pid) ->
  global:register_name(NodeName, Pid).

wakeup(State) ->
  %%Finde akmg in basic edges
  Akmg = lists:foldl(
    fun(Edge, Akmg) ->  case Edge#edge.weight < Akmg#edge.weight of
                          true -> Edge;
                          false -> Akmg
                        end
    end,
    State#state.edges
  ),

  %%Makiere Edge als branch
  AsBranch = Akmg#edge { type = branch },
  NewEdgeList = util:replace_edge(State#state.edges, Akmg, AsBranch),

  State#state { edges = NewEdgeList, status = found }.

handle_initiate_message(State, Level, FragName, NodeState, SourceEdge) ->
  Edge = util:get_edge_by_neighbour_edge(SourceEdge),
  LState = State#state {
                        fragment_level = Level,
                        fragment_name = FragName,
                        status = NodeState,
                        in_branch = Edge,
                        best_edge = undefined,
                        best_weight = infinity
                       },

  BranchList = list:filter(
                fun(Elem) -> (Elem /= Edge) and (Elem#edge.type == branch) end,
                LState#state.edges
              ),

  NewFindCount =  case NodeState == find of
                    true ->
                      LState#state.find_count + length(BranchList);
                    false ->
                      LState#state.find_count
                  end,

  lists:foreach(
    fun(Edge) -> send_initiate_message(Level, FragName, NodeState, Edge) end,
    BranchList ),

  AfterState = LState#state { find_count = NewFindCount },

  case NodeState == find of
    true -> test(AfterState);
    false -> AfterState
  end.

send_initiate_message(FragmentLevel, FragmentName, NodeState, Edge) ->
  get_target_pid(Edge) ! {initiate, FragmentLevel, FragmentName, NodeState, edge_to_tuple(Edge)}.


test(State) ->
  BasicEdges = lists:filter(
                  fun(Edge) -> Edge#edge.type == basic end,
                  State#state.edges),

  case length(BasicEdges) > 0 of
    true ->
      TestEdge = lists:foldl(
                fun(Edge, Akmg) ->  case Edge#edge.weight < Akmg#edge.weight of
                                      true -> Edge;
                                      false-> Akmg
                                    end
                end,
                BasicEdges),
      send_test_message(State#state.fragment_level, State#state.fragment_name, TestEdge),
      State#state { test_edge = TestEdge };
    false ->
      report(State#state { test_edge = undefined })
  end.

send_test_message(FragmentLevel, FragmentName, Edge) ->
  get_target_pid(Edge) ! { test, FragmentLevel, FragmentName, edge_to_tuple(Edge) }.

handle_test_message(State, Level, FragName, Neighbour_edge) ->
  %%Fallunterscheidung:
  {Weight, Edge} = util:get_edge_by_neighbour_edge(Neighbour_edge),

  if FragName == State#state.fragment_name ->
    %%Kante als rejected makieren
    NewEdge = Edge#edge { type = rejected},
    Modified_edge_list = util:replace_edge(State#state.edges, Edge, NewEdge),
    NewState = State#state { edges = Modified_edge_list },

    %%verschicke rejected nachricht
    Send_edge = {
                  NewEdge#edge.weight,
                  NewEdge#edge.node_1#node.name,
                  NewEdge#edge.node_2#node.name
                },

    get_target_pid(NewEdge) ! {reject, Send_edge},
    %%ausnahme - s3 TODO what?
    NewState;
   FragName /= State#state.fragment_name, State#state.fragment_level >= Level ->
    %%sende accept über die kante
    Send_edge = {
                  Edge#edge.weight,
                  Edge#edge.node_1,
                  Edge#edge.node_2
                },
    get_target_pid(Edge) ! {accept, Send_edge},
    State;
  FragName /= State#state.fragment_name, State#state.fragment_level < Level ->
    %%antwort verzögern bzw nicht antworten
    State
  end.

handle_accept_message(State, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(NeighbourEdge),
  {NewBestWeight, NewBestEdge} = case Edge#edge.weight < State#state.best_weight of
                                   true -> {Edge#edge.weight, Edge};
                                   false -> {State#state.best_weight, State#state.best_edge}
                                 end,
  NewState = State#state {
               test_edge = undefined ,
               best_weight = NewBestWeight,
               best_edge = NewBestEdge
              },
  report(NewState).

handle_reject_message(State, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(NeighbourEdge),
  case Edge#edge.type of
    basic ->
      NewEdge = Edge#edge{ type = rejected },
      NewState = State#state { edges = util:replace_edge(State#state.edges, Edge, NewEdge) },
      test(NewState);
    _ -> State
  end.

handle_report_message(State, Weight, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(NeighbourEdge),
  case State#state.in_branch /= Edge of
    true ->
      {NewBestWeight, NewBestEdge} = case Weight < State#state.best_weight of
                                       true -> {Weight, Edge};
                                       false -> {State#state.best_weight, State#state.best_edge}
                                     end,
      NewState = State#state {
                   find_count = State#state.find_count - 1,
                   best_weight = NewBestWeight,
                   best_edge = NewBestEdge
                  },
      report(NewState);
    false ->
      case State#state.status of
        find -> self() ! { report, Weight, edge_to_tuple(NeighbourEdge) };
        _ -> case Weight > State#state.best_weight of
               true -> change_root(State);
               false -> case (State#state.best_weight == infinity) and (Weight == infinity) of
                          true -> log("ending, MST found. i guess :)", [])
                        end
             end
      end,
      State
  end.

report(State) ->
  case (State#state.find_count == 0) and (State#state.test_edge == undefined) of
    true ->
      NewState = State#state { status = found },
      get_target_pid(State#state.in_branch) ! { report, State#state.best_weight, edge_to_tuple(State#state.in_branch) },
      NewState;
    false -> State
  end.

handle_changeroot_mesage(State) ->
  change_root(State).

change_root(State) ->
  BestEdge = State#state.best_edge,
  case BestEdge#edge.type of
    branch -> get_target_pid(BestEdge) ! { changeroot, edge_to_tuple(BestEdge) };
    _ -> get_target_pid(BestEdge) ! { connect, State#state.fragment_level, edge_to_tuple(BestEdge) }
  end,
  State.

handle_connect_message(State, Level, Edge) ->
  NewState =  case State#state.status == sleeping of
                true -> wakeup(State)
              end,

  LocalEdge = util:get_edge_by_neighbour_edge(NewState#state.edges, Edge),

    if
      Level < NewState#state.fragment_level ->
        %%Makiere LocalEdge als branch
        Branch = LocalEdge#edge { type = branch },
        AktState = State#state { edges = util:replace_edge(State#state.edges,
                                                           LocalEdge,
                                                           Branch)},
        %%Sende initiate ueber LocalEdge
        send_initiate_message(AktState#state.fragment_level,
                              AktState#state.fragment_name,
                              AktState#state.status,
                              Branch),


        case AktState#state.status == find of
          true ->
            %%Addiere 1 auf find-count
            AktState#state { find_count = AktState#state.find_count + 1 };
          false ->
            NewState
        end;
      LocalEdge#edge.type == basic ->
        %%Place message on end of queue
        self() ! {connect, Level, Edge},
        NewState;
      true ->
        %%Sende Initiate
        send_initiate_message(NewState#state.fragment_level + 1,
                              LocalEdge#edge.weight,
                              find,
                              LocalEdge),
        NewState
    end.
