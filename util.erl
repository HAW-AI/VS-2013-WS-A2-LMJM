 -module(util).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%%Records einbinden
-include_lib("datastructures.hrl").

%%Ersetzt ein Element in
replace_edge(List, Old_elem, New_elem) ->
  List_tmp = List -- [Old_elem],
  List_tmp ++ [New_elem].


get_edge_by_neighbour_edge(Edge_list, {Weight, Nodex, Nodey}) ->
  List_tmp = lists:filter(
                fun(Elem) -> (Elem#edge.weight == Weight) and
                             (Elem#edge.node_1 == Nodey) and
                             (Elem#edge.node_2 == Nodex)
                end,
                Edge_list),
  case length(List_tmp) >  0 of
    true -> lists:nth(1, List_tmp);
    false -> not_found
  end.

log(Format, Data) ->
  io:format(Format ++ "~n", Data).

get_best_edge_from_basic_edges(EdgeList) ->
  Edges = lists:filter(
                  fun(Edge) -> Edge#edge.type == basic end,
                  EdgeList),

  [ FirstEdge | _ ] = Edges,
  lists:foldl(
           fun(Edge, Akmg) -> case Edge#edge.weight < Akmg#edge.weight of
                                true -> Edge;
                                false -> Akmg
                              end
           end,
           FirstEdge,
           Edges
          ).

%%Testcases
replace_edge_test()->
  Edge_1 = #edge {
    node_1 = eins,
    node_2 = zwei,
    weight = 1,
    type   = basic
  },

  Edge_2 = #edge {
    node_1 = drei,
    node_2 = vier,
    weight = 4,
    type   = rejected
  },

  Edge_1_changed = #edge {
    node_1 = eins,
    node_2 = zwei,
    weight = 1,
    type   = branch
  },

  Edge_2_changed = #edge {
    node_1 = drei,
    node_2 = vier,
    weight = 4,
    type   = rejected
  },

  Source_list = [Edge_1, Edge_2],
  Destination_list_1 = [Edge_1, Edge_2_changed],
  Destination_list_2 = [Edge_2, Edge_1_changed],

  ?assertEqual(Destination_list_1, replace_edge(Source_list, Edge_2, Edge_2_changed)),
  ?assertEqual(Destination_list_2, replace_edge(Source_list, Edge_1, Edge_1_changed)).

get_edge_by_neighbour_edge_test() ->
  %Edge_list, {Weight, Nodex, Nodey}
  Edge_1 = #edge {
    node_1 = eins,
    node_2 = zwei,
    weight = 1,
    type   = basic
  },

  Edge_2 = #edge {
    node_1 = drei,
    node_2 = vier,
    weight = 4,
    type   = rejected
  },

  Edge_list = [Edge_1, Edge_2],

  ?assertEqual(Edge_1, get_edge_by_neighbour_edge(Edge_list, {1, zwei, eins})),
  ?assertEqual(Edge_2, get_edge_by_neighbour_edge(Edge_list, {4, vier, drei})).

get_akmg_from_basic_edges_test() ->
  Edge_1 = #edge {node_1 = eins, node_2 = zwei, weight = 2, type = basic},
  Edge_2 = #edge {node_1 = drei, node_2 = vier, weight = 4, type = branch},
  Edge_3 = #edge {node_1 = fuenf, node_2 = sechs, weight = 6, type = basic},

  EdgeList = [Edge_1, Edge_2, Edge_3],

  ?assertEqual(Edge_1, get_best_edge_from_basic_edges(EdgeList)).
