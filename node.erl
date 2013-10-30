-module(node).
-export([start/1]).

-include_lib("datastructures.hrl").
-import(util, [log/2]).

-define(INFINITY, 999999999999).

start(State) ->
  yes = register_node(State#state.name, self()),
  loop(State).

loop(State) ->
  log(
"~p (~p)
  Fragment Name ~p
  Fragment Level ~p,
  Edges ~p
  Best Edge ~p
  Best Weight ~p
  In Branch ~p
  Test Edge ~p",
    [
     State#state.name,
     State#state.status,
     State#state.fragment_name,
     State#state.fragment_level,
     State#state.edges,
     State#state.best_edge,
     State#state.best_weight,
     State#state.in_branch,
     State#state.test_edge
    ]),

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
  BestEdge = util:get_best_edge_from_basic_edges(State#state.edges),

  NewEdgeList = util:replace_edge(
                  State#state.edges,
                  BestEdge,
                  BestEdge#edge { type = branch }),

  log("~p sending connect to ~p", [State#state.name, BestEdge#edge.node_2]),
  get_target_pid(BestEdge) ! { connect, 0, edge_to_tuple(BestEdge) },

  State#state {
    edges = NewEdgeList,
    fragment_level = 0,
    status = found,
    find_count = 0
   }.

handle_connect_message(State, Level, NeighbourEdge) ->
  NewState = case State#state.status == sleeping of
               true -> wakeup(State);
               _ -> State
             end,
  Edge = util:get_edge_by_neighbour_edge(NewState#state.edges, NeighbourEdge),

  if
    Level < NewState#state.fragment_level ->
      Branch = Edge#edge { type = branch },
      AktState = State#state { edges = util:replace_edge(State#state.edges,
                                                         Edge,
                                                         Branch)},

      send_initiate_message(AktState,
                            AktState#state.fragment_level,
                            AktState#state.fragment_name,
                            AktState#state.status,
                            Branch),

      case AktState#state.status == find of
        true ->
          AktState#state { find_count = AktState#state.find_count + 1 };
        false ->
          AktState
      end;

    Edge#edge.type == basic ->
      log("~p relaying connect to myself", [State#state.name]),
      self() ! {connect, Level, NeighbourEdge},
      NewState;
    true ->
      send_initiate_message(NewState,
                            NewState#state.fragment_level + 1,
                            Edge#edge.weight,
                            find,
                            Edge),
      NewState
  end.

handle_initiate_message(State, Level, FragName, NodeStatus, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),

  BranchList = lists:filter(
                 fun(Elem) -> (Elem /= Edge) and (Elem#edge.type == branch) end,
                 State#state.edges
                ),

  NewFindCount = case NodeStatus == find of
                   true -> State#state.find_count + length(BranchList);
                   false -> State#state.find_count
                 end,

  lists:foreach(
    fun(EdgeElem) -> send_initiate_message(State, Level, FragName, NodeStatus, EdgeElem) end,
    BranchList
   ),

  NewState = State#state {
                        status = NodeStatus,
                        fragment_level = Level,
                        fragment_name = FragName,
                        in_branch = Edge,
                        best_edge = undefined,
                        best_weight = ?INFINITY,
                        find_count = NewFindCount
                       },

  case NodeStatus == find of
    true -> test(NewState);
    false -> NewState
  end.

send_initiate_message(State, FragmentLevel, FragmentName, NodeState, Edge) ->
  log("~p sending initiate to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! {initiate, FragmentLevel, FragmentName, NodeState, edge_to_tuple(Edge)}.


test(State) ->
  BasicEdges = lists:filter(
                  fun(Edge) -> Edge#edge.type == basic end,
                  State#state.edges),

  case length(BasicEdges) > 0 of
    true ->
      TestEdge = util:get_best_edge_from_basic_edges(BasicEdges),
      send_test_message(State, State#state.fragment_level, State#state.fragment_name, TestEdge),
      State#state { test_edge = TestEdge };
    false ->
      report(State#state { test_edge = undefined })
  end.

send_test_message(State, FragmentLevel, FragmentName, Edge) ->
  log("~p sending test to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! { test, FragmentLevel, FragmentName, edge_to_tuple(Edge) }.

handle_test_message(InState, Level, FragName, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(InState#state.edges, NeighbourEdge),
  State = case InState#state.status == sleeping of
            true -> wakeup(InState);
            false -> InState
          end,
  IfState = if
    Level > State#state.fragment_level ->
      log("~p relaying test to myself", [State#state.name]),
      self() ! {test, Level, FragName, NeighbourEdge},
      State;
    FragName /= State#state.fragment_name ->
      log("~p sending accept to ~p", [State#state.name, Edge#edge.node_2]),
      get_target_pid(Edge) ! {accept, edge_to_tuple(Edge)},
      State;
    true ->
      case Edge#edge.type == basic of
        true ->
          Rejected = Edge#edge { type = rejected },
          State#state { edges = util:replace_edge(State#state.edges, Edge, Rejected) };
        false -> State
      end
    end,

    case IfState#state.test_edge /= Edge of
      true ->
        log("~p sending reject to ~p", [State#state.name, Edge#edge.node_2]),
        get_target_pid(Edge) ! {reject, edge_to_tuple(Edge)},
        IfState;
      false ->
        test(IfState)
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
  NewState = case Edge#edge.type of
    basic ->
      NewEdge = Edge#edge{ type = rejected },
      State#state { edges = util:replace_edge(State#state.edges, Edge, NewEdge) };
    _ -> State
  end,
  test(NewState).

report(State) ->
  case (State#state.find_count == 0) and (State#state.test_edge == undefined) of
    true ->
      NewState = State#state { status = found },
      log("~p sending report to ~p", [State#state.name, State#state.in_branch#edge.node_2]),
      get_target_pid(State#state.in_branch) ! { report, State#state.best_weight, edge_to_tuple(State#state.in_branch) },
      NewState;
    false -> State
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
        find ->
          log("~p relaying report to myself", [State#state.name]),
          self() ! { report, Weight, NeighbourEdge };
        _ ->
          case Weight > State#state.best_weight of
            true -> change_root(State);
            false -> case (State#state.best_weight == ?INFINITY) and (Weight == ?INFINITY) of
                       true -> log("ending, MST found. i guess :)", []);
                       false -> noop
                     end
          end
      end,
      State
  end.

change_root(State) ->
  BestEdge = State#state.best_edge,
  case BestEdge#edge.type of
    branch ->
      log("~p sending changeroot to ~p", [State#state.name, BestEdge#edge.node_2]),
      get_target_pid(BestEdge) ! { changeroot, edge_to_tuple(BestEdge) };
    _ ->
      log("~p sending connect to ~p", [State#state.name, BestEdge#edge.node_2]),
      get_target_pid(BestEdge) ! { connect, State#state.fragment_level, edge_to_tuple(BestEdge) }
  end,
  State#state { edges = util:replace_edge(State#state.edges, BestEdge, BestEdge#edge { type = branch }) }.

handle_changeroot_mesage(State) ->
  change_root(State).

