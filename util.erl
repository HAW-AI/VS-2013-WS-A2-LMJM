-module(util).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%%Records einbinden
-include_lib("datastructures.hrl").

%%Ersetzt ein Element in
replace_element(List, Old_elem, New_elem) ->
  List_tmp = List -- [Old_elem],
  List_tmp ++ [New_elem].


get_edge_by_neighbour_edge(Edge_list, {Weight, Nodex, Nodey}) ->
  List_tmp = lists:filter(
                fun({_, Elem}) -> (Elem#edge.weight == Weight) and
                             (Elem#edge.node_1#node.name == Nodey) and
                             (Elem#edge.node_2#node.name == Nodex)
                end,
                Edge_list),
  case length(List_tmp) >  0 of
    true -> lists:nth(1, List_tmp);
    false -> not_found
  end.


%%Testcases
replace_edge_test()->
  Edge_1 = #edge {
    node_1 = #node { name = "eins", pid = undefined },
    node_2 = #node { name = "zwei", pid = undefined },
    weight = 1,
    type   = basic
  },

  Edge_2 = #edge {
    node_1 = #node { name = "drei", pid = undefined },
    node_2 = #node { name = "vier", pid = undefined },
    weight = 4,
    type   = rejected
  },

  Edge_1_changed = #edge {
    node_1 = #node { name = "eins", pid = undefined },
    node_2 = #node { name = "zwei", pid = undefined },
    weight = 1,
    type   = branch
  },

  Edge_2_changed = #edge {
    node_1 = #node { name = "drei", pid = undefined },
    node_2 = #node { name = "vier", pid = undefined },
    weight = 4,
    type   = rejected
  },

  Source_list = [Edge_1, Edge_2],
  Destination_list_1 = [Edge_1, Edge_2_changed],
  Destination_list_2 = [Edge_2, Edge_1_changed],

  ?assertEqual(Destination_list_1, replace_element(Source_list, Edge_2, Edge_2_changed)),
  ?assertEqual(Destination_list_2, replace_element(Source_list, Edge_1, Edge_1_changed)).

get_edge_by_neighbour_edge_test() ->
  %Edge_list, {Weight, Nodex, Nodey}
  Edge_1 = {"1", #edge {
    node_1 = #node { name = "eins", pid = undefined },
    node_2 = #node { name = "zwei", pid = undefined },
    weight = "1",
    type   = basic
  }},

  Edge_2 = {"4", #edge {
    node_1 = #node { name = "drei", pid = undefined },
    node_2 = #node { name = "vier", pid = undefined },
    weight = "4",
    type   = rejected
  }},

  Edge_list = [Edge_1, Edge_2],

  ?assertEqual(Edge_1, get_edge_by_neighbour_edge(Edge_list, {"1", "zwei", "eins"})),
  ?assertEqual(Edge_2, get_edge_by_neighbour_edge(Edge_list, {"4", "vier", "drei"})).
