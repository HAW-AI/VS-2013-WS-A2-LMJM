-module(node).
-export([start/1]).

-include_lib("datastructures.hrl").
-import(util, [log/2]).

-define(INFINITY, 999999999999).

start(State) ->
  yes = register_node(State#state.name, self()),
  loop(State).

loop(State) ->
  log_state(State),
  timer:sleep(100),

  receive
    wakeup ->
      log("~p received wakeup", [State#state.name]),
      NewState = wakeup(State),
      loop(NewState);
    {initiate,Level,FragName,NodeState,Edge} ->
      log("~p received initiate on edge ~p", [State#state.name, Edge]),
      NewState = handle_initiate(State, Level, FragName, NodeState, Edge),
      loop(NewState);
    {test,Level,FragName,Edge} ->
      log("~p received test on edge ~p", [State#state.name, Edge]),
      NewState = handle_test(State, Level, FragName, Edge),
      loop(NewState);
    {accept,Edge} ->
      log("~p received accept on edge ~p", [State#state.name, Edge]),
      NewState = handle_accept(State, Edge),
      loop(NewState);
    {reject,Edge} ->
      log("~p received reject on edge ~p", [State#state.name, Edge]),
      NewState = handle_reject(State, Edge),
      loop(NewState);
    {report,Weight,Edge} ->
      log("~p received report on edge ~p", [State#state.name, Edge]),
      NewState = handle_report(State, Weight, Edge),
      loop(NewState);
    {changeroot,Edge} ->
      log("~p received changeroot on edge ~p", [State#state.name, Edge]),
      NewState = handle_changeroot(State),
      loop(NewState);
    {connect,Level,Edge} ->
      log("~p received connect on edge ~p", [State#state.name, Edge]),
      NewState = handle_connect(State, Level, Edge),
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

  send_connect(State, 0, BestEdge),

  State#state {
    edges = NewEdgeList,
    fragment_level = 0,
    status = found,
    find_count = 0
   }.


handle_connect(State, Level, NeighbourEdge) ->
  NewState = case State#state.status == sleeping of
               true -> wakeup(State);
               _ -> State
             end,

  Edge = util:get_edge_by_neighbour_edge(NewState#state.edges, NeighbourEdge),
  if
    Level < NewState#state.fragment_level ->
      Branch = Edge#edge { type = branch },
      AktState = State#state { edges = util:replace_edge(State#state.edges, Edge, Branch)},

      send_initiate(AktState,
                            AktState#state.fragment_level,
                            AktState#state.fragment_name,
                            AktState#state.status,
                            Branch),

      case AktState#state.status of
        find ->
          AktState#state { find_count = AktState#state.find_count + 1 };
        _ ->
          AktState
      end;

    true ->
      case Edge#edge.type of
        basic ->
          resend(State, {connect, Level, NeighbourEdge}),
          NewState;
        _ ->
          send_initiate(NewState, NewState#state.fragment_level + 1, Edge#edge.weight, find, Edge),
          NewState
      end
  end.


handle_initiate(State, Level, FragName, NodeStatus, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),

  BranchList = lists:filter(
                 fun(Elem) -> (not util:are_edges_equal(Elem, Edge)) and (Elem#edge.type == branch) end,
                 State#state.edges
                ),

  NewFindCount = case NodeStatus of
                   find -> State#state.find_count + length(BranchList);
                   _ -> State#state.find_count
                 end,

  lists:foreach(
    fun(EdgeElem) -> send_initiate(State, Level, FragName, NodeStatus, EdgeElem) end,
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

  case NodeStatus of
    find -> test(NewState);
    _ -> NewState
  end.


test(State) ->
  BasicEdges = lists:filter(
                  fun(Edge) -> Edge#edge.type == basic end,
                  State#state.edges),

  case length(BasicEdges) > 0 of
    true ->
      TestEdge = util:get_best_edge_from_basic_edges(BasicEdges),
      send_test(State, State#state.fragment_level, State#state.fragment_name, TestEdge),
      State#state { test_edge = TestEdge };
    false ->
      report(State#state { test_edge = undefined })
  end.


handle_test(InState, Level, FragName, NeighbourEdge) ->
  State = case InState#state.status of
            sleeping -> wakeup(InState);
            _ -> InState
          end,

  if
    Level > State#state.fragment_level ->
      resend(State, {test, Level, FragName, NeighbourEdge}),
      State;
    true ->
      Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),

      if FragName /= State#state.fragment_name ->
        send_accept(State, Edge),
        State;
      true ->
        NewState = case Edge#edge.type of
          basic ->
            Rejected = Edge#edge { type = rejected },
            State#state { edges = util:replace_edge(State#state.edges, Edge, Rejected) };
          _ -> State
        end,

        case not util:are_edges_equal(NewState#state.test_edge, Edge) of
          true ->
            send_reject(NewState, Edge),
            NewState;
          false ->
            test(NewState)
        end
      end
  end.


handle_accept(State, NeighbourEdge) ->
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


handle_reject(State, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),
  NewState = case Edge#edge.type of
    basic ->
      NewEdge = Edge#edge{ type = rejected },
      State#state { edges = util:replace_edge(State#state.edges, Edge, NewEdge) };
    _ -> State
  end,
  test(NewState).


report(State) ->
  case {State#state.find_count, State#state.test_edge} of
    {0, undefined} ->
      NewState = State#state { status = found },
      send_report(State, State#state.best_weight, State#state.in_branch),
      NewState;
    _ -> State
  end.


handle_report(State, Weight, NeighbourEdge) ->
  Edge = util:get_edge_by_neighbour_edge(State#state.edges, NeighbourEdge),
  case util:are_edges_equal(State#state.in_branch, Edge) of
    false ->
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
    true ->
      case State#state.status of
        find ->
          resend(State, {report, Weight, NeighbourEdge}),
          State;
        _ ->
          case Weight > State#state.best_weight of
            true ->
              change_root(State);
            false -> case {State#state.best_weight, Weight} of
                       {?INFINITY, ?INFINITY} ->
                         log("~p ending, MST found. i guess :)", [State#state.name]),
                         exit(normal);
                       _ -> State
                     end
          end
      end
  end.


change_root(State) ->
  BestEdge = State#state.best_edge,
  case BestEdge#edge.type of
    branch ->
      send_changeroot(State, BestEdge),
      State;
    _ ->
      send_connect(State, State#state.fragment_level, BestEdge),
      NewEdgeList = util:replace_edge(State#state.edges, BestEdge, BestEdge#edge { type = branch }),
      State#state { edges = NewEdgeList }
  end.


handle_changeroot(State) ->
  change_root(State).

% send methods


send_connect(State, Level, Edge) ->
  log("~p sending connect to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! { connect, Level, edge_to_tuple(Edge) }.

send_initiate(State, FragmentLevel, FragmentName, NodeStatus, Edge) ->
  log("~p sending initiate to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! {initiate, FragmentLevel, FragmentName, NodeStatus, edge_to_tuple(Edge)}.

send_test(State, FragmentLevel, FragmentName, Edge) ->
  log("~p sending test to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! { test, FragmentLevel, FragmentName, edge_to_tuple(Edge) }.

send_accept(State, Edge) ->
  log("~p sending accept to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! {accept, edge_to_tuple(Edge)}.

send_reject(State, Edge) ->
  log("~p sending reject to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! {reject, edge_to_tuple(Edge)}.

send_report(State, BestWeight, Edge) ->
  log("~p sending report to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! { report, BestWeight, edge_to_tuple(Edge) }.

send_changeroot(State, Edge) ->
  log("~p sending changeroot to ~p", [State#state.name, Edge#edge.node_2]),
  get_target_pid(Edge) ! { changeroot, edge_to_tuple(Edge) }.

resend(State, Payload) ->
  [Message | _] = tuple_to_list(Payload),
  log("~p relaying ~p to itself", [State#state.name, Message]),
  timer:sleep(300),
  self() ! Payload.

log_state(State) ->
  log(
"~p (~p) ~p
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
     calendar:local_time(),
     State#state.fragment_name,
     State#state.fragment_level,
     State#state.edges,
     State#state.best_edge,
     State#state.best_weight,
     State#state.in_branch,
     State#state.test_edge
    ]).
