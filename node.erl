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
get_pid_of_neighbour_nodes([{Weight, Edge} | Tail], NewEdgeList) ->
  Pid = get_pid_by_name(Edge#edge.node_2#node.name),

  NewNode = Edge#edge.node_2#node { pid = Pid },
  NewEdge = Edge#edge {node_2 = NewNode },
  NewList = NewEdgeList ++ [NewEdge],
  get_pid_of_neighbour_nodes(Tail, NewList).


handle_initiate_message(State, Level, FragName, NodeState, SourceEdge) ->
  %%Finde akmg in basic edges
  Akmg = lists:foldl(
    fun(Edge, Akmg) ->  case Edge#edge.weight < Akmg#edge.weight of
                          true -> Edge;
                          false -> Akmg
                        end
    end,
    State#state.edges
  ),

  %%Sende Test
  EdgeTuple = {Akmg#edge.weight, Akmg#edge.node_1#node.name, Akmg#edge.node_2#node.name},
  Akmg#edge.node_2#node.pid ! {test,State#state.fragment_level,Akmg#edge.weight,EdgeTuple},
  %%State unverändert zurückgeben
  State.

get_pid_by_name(NodeName) -> global:whereis_name(NodeName).

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

handle_report_message(State, Weight, Edge) ->
  State.

handle_changeroot_mesage(State, Edge) ->
  State.

handle_connect_message(State, Level, Edge) ->
  State.

%%Testcases
