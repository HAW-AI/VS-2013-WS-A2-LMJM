-module(node).
-export([start/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("datastructures.hrl").


start(State) ->
  register_node(State#state.name, self()),
  NewState = get_pid_of_neighbour_nodes(State),
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


log(Format, Data) ->
  io:format("node: " ++ Format ++ "~n", Data).


%%Registriert den NodeName im Netzwerk
register_node(NodeName, Pid) ->
  global:register_name(NodeName, Pid).

%%Für alle Nachbar nodes in basic edge list die PID ermitteln
get_pid_of_neighbour_nodes(State) ->
  NewEdgeList = get_pid_of_neighbour_nodes(State#state.edges, []),
  State#state { edges = NewEdgeList }.


get_pid_of_neighbour_nodes([], NewEdgeList) ->
  NewEdgeList;
get_pid_of_neighbour_nodes([{Weight, Edge} | Tail], NewEdgeList) ->
  Pid = get_pid_by_name(Edge#edge.node_2#node.name),

  NewNode = Edge#edge.node_2#node { pid = Pid },
  NewEdge = Edge#edge {node_2 = NewNode },
  NewList = NewEdgeList ++ [NewEdge],
  get_pid_of_neighbour_nodes(Tail, NewList).


get_pid_by_name(NodeName) -> global:whereis_name(NodeName).


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
                        best_edge = nil,
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
  EdgeTuple = { Edge#edge.weight, Edge#edge.node_1#node.name, Edge#edge.node_2#node.name },
  Edge#edge.node_2#node.pid ! {initiate, FragmentLevel, FragmentName, NodeState, EdgeTuple}.


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
  EdgeTuple = {Edge#edge.weight, Edge#edge.node_1#node.name, Edge#edge.node_2#node.name },
  Edge#edge.node_2#node.pid ! {test, FragmentLevel, FragmentName, EdgeTuple}.

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

    NewEdge#edge.node_2#node.pid ! {reject, Send_edge},
    %%ausnahme - s3 TODO what?
    NewState;
   FragName /= State#state.fragment_name, State#state.fragment_level >= Level ->
    %%sende accept über die kante
    Send_edge = {
                  Edge#edge.weight,
                  Edge#edge.node_1,
                  Edge#edge.node_2
                },
    Edge#edge.node_2#node.pid ! {accept, Send_edge},
    State;
  FragName /= State#state.fragment_name, State#state.fragment_level < Level ->
    %%antwort verzögern bzw nicht antworten
    State
  end.

handle_accept_message(State, Edge) ->
  State.

handle_reject_message(State, Edge) ->
  State.

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
        find -> self() ! { report, Weight, NeighbourEdge };
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
      State#state.in_branch#edge.node_2#node.pid ! { report, State#state.best_weight, State#state.in_branch },
      NewState;
    false -> State
  end.

handle_changeroot_mesage(State) ->
  change_root(State).

change_root(State) ->
  BestEdge = State#state.best_edge,
  case BestEdge#edge.type of
    branch -> BestEdge#edge.node_2#node.pid ! { changeroot, BestEdge };
    _ -> BestEdge#edge.node_2#node.pid ! { connect, State#state.fragment_level, BestEdge }
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
        %%Sende initiate ueber LocalEdge
        case NewState#state.status == find of
          true ->
            %%Addiere 1 auf find-count
            NewState;
          false ->
            NewState
        end;
      LocalEdge#edge.type == branch ->
        %%Place message on end of queue
        NewState;
      true ->
        %%Sende Initiate
        NewState
    end.

%%Testcases  LState#state { find_count = NewFindCount }.
