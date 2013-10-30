-module(node).
-export([start/1]).

-include_lib("datastructures.hrl").
-import(util, [log/2]).


start(State) ->
  yes = register_node(State#state.name, self()),
  loop(State).

loop(State) ->
  log("~p", [State]),

  receive
    wakeup ->
      log("~p received wakeup", [State#state.name]),
      NewState = wakeup(State),
      loop(NewState);
    {initiate,Level,FragName,NodeState,Edge} ->
      log("~p received initiate on edge ~p", [State#state.name, Edge]),
      NewState = handle_initiate_message(State, Level, FragName, NodeState, Edge),
      loop(NewState);
    {test,Level,FragName,Edge} ->
      log("~p received test on edge ~p", [State#state.name, Edge]),
      NewState = handle_test_message(State, Level, FragName, Edge),
      loop(NewState);
    {accept,Edge} ->
      log("~p received accept on edge ~p", [State#state.name, Edge]),
      NewState = handle_accept_message(State, Edge),
      loop(NewState);
    {reject,Edge} ->
      log("~p received reject on edge ~p", [State#state.name, Edge]),
      NewState = handle_reject_message(State, Edge),
      loop(NewState);
    {report,Weight,Edge} ->
      log("~p received report on edge ~p", [State#state.name, Edge]),
      NewState = handle_report_message(State, Weight, Edge),
      loop(NewState);
    {changeroot,Edge} ->
      log("~p received changeroot on edge ~p", [State#state.name, Edge]),
      NewState = handle_changeroot_mesage(State),
      loop(NewState);
    {connect,Level,Edge} ->
      log("~p received connect on edge ~p", [State#state.name, Edge]),
      NewState = handle_connect_message(State, Level, Edge),
      loop(NewState)
  end.

get_target_pid(Edge) ->
  global:whereis_name(Edge#edge.node_2).

edge_to_tuple(Edge) ->
  { Edge#edge.weight, Edge#edge.node_1, Edge#edge.node_2 }.

register_node(NodeName, Pid) ->
  global:register_name(NodeName, Pid).

wakeup(State) ->
  [ FirstEdge | _ ] = State#state.edges,
  Akmg = lists:foldl(
           fun(Edge, Akmg) -> case Edge#edge.weight < Akmg#edge.weight of
                                true -> Edge;
                                false -> Akmg
                              end
           end,
           FirstEdge,
           State#state.edges
          ),

  %%Makiere Edge als branch
  AsBranch = Akmg#edge { type = branch },
  NewEdgeList = util:replace_edge(State#state.edges, Akmg, AsBranch),

  get_target_pid(Akmg) ! { connect, State#state.fragment_level, edge_to_tuple(Akmg) },
  State#state { edges = NewEdgeList, status = found }.

handle_initiate_message(State, Level, FragName, NodeState, SourceEdge) ->
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, SourceEdge),
  LState = State#state {
                        fragment_level = Level,
                        fragment_name = FragName,
                        status = NodeState,
                        in_branch = Edge,
                        best_edge = undefined,
                        best_weight = infinity
                       },

  BranchList = lists:filter(
                fun(Elem) -> (Elem /= Edge) and (Elem#edge.type == branch) end,
                LState#state.edges
              ),

  NewFindCount =  case NodeState == find of
                    true -> LState#state.find_count + length(BranchList);
                    false -> LState#state.find_count
                  end,

  lists:foreach(
    fun(EdgeElem) -> send_initiate_message(Level, FragName, NodeState, EdgeElem) end,
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
      [ FirstEdge | _ ] = BasicEdges,
      TestEdge = lists:foldl(
                fun(Edge, Akmg) ->  case Edge#edge.weight < Akmg#edge.weight of
                                      true -> Edge;
                                      false-> Akmg
                                    end
                end,
                FirstEdge,
                BasicEdges),
      send_test_message(State#state.fragment_level, State#state.fragment_name, TestEdge),
      State#state { test_edge = TestEdge };
    false ->
      report(State#state { test_edge = undefined })
  end.

send_test_message(FragmentLevel, FragmentName, Edge) ->
  get_target_pid(Edge) ! { test, FragmentLevel, FragmentName, edge_to_tuple(Edge) }.

handle_test_message(InState, Level, FragName, NeighbourEdge) ->
  State = case InState#state.status == sleeping of
            true -> wakeup(InState);
            false -> InState
          end,
  if
    Level > State#state.fragment_level ->
      self() ! {test, Level, FragName, NeighbourEdge},
      State;
    FragName /= State#state.fragment_name ->
      NEdge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),
      get_target_pid(NEdge) ! {accept, NeighbourEdge},
      State;
    true ->
      NEdge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),
      case NEdge#edge.type == basic of
        true ->
          Rejected = NEdge#edge { type = rejected },
          AktState = State#state { edges = util:replace_edge(State#state.edges,
                                                             NEdge,
                                                             Rejected)},
          case AktState#state.test_edge /= Rejected of
            true ->
              get_target_pid(Rejected) ! {reject, edge_to_tuple(Rejected)},
              AktState;
            false ->
              test(AktState)
          end;
        false -> State
        end
      end.

handle_accept_message(State, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),
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
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),
  case Edge#edge.type of
    basic ->
      NewEdge = Edge#edge{ type = rejected },
      NewState = State#state { edges = util:replace_edge(State#state.edges, Edge, NewEdge) },
      test(NewState);
    _ -> State
  end.

handle_report_message(State, Weight, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),
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
  NewState = case State#state.status == sleeping of
               true -> wakeup(State);
               _ -> State
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
