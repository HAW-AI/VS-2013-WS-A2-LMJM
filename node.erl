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
    {changeroot,Edge} ->
      NewState = handle_changeroot_mesage(State, Edge),
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
get_pid_of_neighbour_nodes([Edge | Tail], NewEdgeList) ->
  Pid = get_pid_by_name(Edge#edge.node_2#node.name),

  NewNode = Edge#edge.node_2#node { pid = Pid },
  NewEdge = Edge#edge {node_2 = NewNode },
  NewList = NewEdgeList ++ [NewEdge],
  get_pid_of_neighbour_nodes(Tail, NewList).


handle_initiate_message(GlobalState, Level, FragName, NodeState, SourceEdge) ->
  %%Finde akmg in basic edges
  Akmg = lists:foldl(
    fun(Edge, Akmg) ->  case Edge#edge.weight < Akmg#edge.weight of
                          true -> Edge;
                          false -> Akmg
                        end
    end,
    GlobalState#state.edges
  ),

  %%Sende Test
  Akmg#edge.node_2#node.pid ! {test,GlobalState#state.fragmentLevel,Akmg#edge.weight,Akmg},
  %%State unverändert zurückgeben
  GlobalState.

get_pid_by_name(NodeName) -> global:whereis_name(NodeName).

handle_test_message(State, Level, FragName, Edge) ->
  %%Fallunterscheidung:
  if FragName == State#state.fragmentName ->
    %%Kante als rejected makieren
    %%verschicke rejected nachricht
    %%ausnahme - s3.
    State;
   FragName /= State#state.fragmentName, State#state.fragmentLevel >= Level ->
    %%sende accept über die kante
    State;
  FragName /= State#state.fragmentName, State#state.fragmentLevel < Level ->
    %%antwort verzögern bzw nicht antworten
    State
  end.

handle_accept_message(State, Edge) ->
  State.

handle_reject_message(State, Edge) ->
  State.

handle_report_message(State, Weight, Edge) ->
  State.

handle_changeroot_mesage(State, Edge) ->
  State.

handle_connect_message(State, Level, Edge) ->
  State.

%%Testcases
