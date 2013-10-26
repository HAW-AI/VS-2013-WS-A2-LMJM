-module(node).
-export([loop/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datastructures.hrl").

loop(State) ->
  NewState = get_pid_of_neighbour_nodes(State),
  lists:foreach(
    fun(_) -> log("~p", [NewState]) end,
    lists:seq(0, 1)
  ),
  io:fread("", "~c").

log(Format, Data) ->
  io:format("node: " ++ Format ++ "~n", Data).


%%Für alle Nachbar nodes in basic edge list die PID ermitteln
get_pid_of_neighbour_nodes(State) ->
  NewEdgeList = get_pid_of_neighbour_nodes(State#state.basic_edges, []),
  State#state { basic_edges = NewEdgeList }.


get_pid_of_neighbour_nodes([], NewEdgeList) ->
  NewEdgeList;
get_pid_of_neighbour_nodes([Edge | Tail], NewEdgeList) ->
  Pid = get_pid_by_name(Edge#edge.node_2#node.name),

  NewNode = Edge#edge.node_2#node { pid = Pid },
  NewEdge = Edge#edge {node_2 = NewNode },
  NewList = NewEdgeList ++ [NewEdge],
  get_pid_of_neighbour_nodes(Tail, NewList).

%%

get_pid_by_name(NodeName) -> global:whereis_name(NodeName).

%%Testcases
