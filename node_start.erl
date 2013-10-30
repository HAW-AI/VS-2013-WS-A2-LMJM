-module(node_start).
-export([main/1, split_string/2]).
-import(string, [rstr/2, len/1, substr/3, sub_string/3, strip/1]).

-include_lib("eunit/include/eunit.hrl").

%%Records einbinden
-include_lib("datastructures.hrl").

main(ConfigFile) ->
  Lines = read_lines(ConfigFile),
  NodeName = get_node_name(ConfigFile),

  %%Generiere lokale umgebung des Nodes/Knoten
  Edges = get_edges_from_config(NodeName, get_config_from_lines(Lines)),

  %%Init state of this node
  NodeState = #state {
    name = NodeName,
    edges = Edges,
    status = sleeping,
    fragment_level = 0,
    fragment_name = undefined
  },

  spawn(fun() -> node:start(NodeState) end).

read_lines(File) ->
  {ok, IoDevice} = file:open(File, read),
  read_lines_recursion(file:read_line(IoDevice), IoDevice, []).

read_lines_recursion({ok, Line}, IoDevice, List) ->
  NewList = lists:append(List, [Line]),
  read_lines_recursion(file:read_line(IoDevice), IoDevice, NewList);
read_lines_recursion(eof, IoDevice, List) ->
  file:close(IoDevice),
  List.

get_node_name(ConfigFile) ->
  Name = case rstr(ConfigFile, "/") > 0 of
    true ->
      Index_1 = rstr(ConfigFile, "/") + 1,
      Index_2 = rstr(ConfigFile, "."),
      substr(ConfigFile, Index_1, Index_2 - Index_1);
    false ->
      Index_1 = rstr(ConfigFile, "."),
      substr(ConfigFile, 1, Index_1 - 1)
  end,
  list_to_atom(Name).



%%Teilt einen string an der stelle, wo char gefunden wird
%%Ist char nicht enthalten wird ein tupel aus {string, ""} zurückgegeben
split_string(String, Char) ->
  case rstr(String, Char) > 0 of
    true ->
      case rstr(String, "\n") > 0 of
        true ->
          {strip(sub_string(String, 1, rstr(String, Char)-1)), strip(sub_string(String, rstr(String, Char)+len(Char), rstr(String, "\n")-1))};
        false ->
          {strip(sub_string(String, 1, rstr(String, Char)-1)), strip(sub_string(String, rstr(String, Char)+len(Char), len(String)))}
      end;
    false ->
      {String, ""}
  end.

get_config_from_lines(Lines) ->
  StringList = [split_string(Line, ",") || Line <- Lines],
  lists:map(fun ({WeightString, NodeString}) ->
                  {Weight, _} = string:to_integer(WeightString),
                  Node = list_to_atom(NodeString),
                  {Weight, Node}
                end,
                StringList).

%%Initialisiert die Edges mit unbekannter PID der nodes
get_edges_from_config(ParentNodeName, Config) ->
  [
   #edge {
    node_1 = #node { name = ParentNodeName, pid = undefined },
    node_2 = #node { name = NodeName, pid = undefined },
    weight = Weight,
    type = basic
  } || {Weight, NodeName} <- Config ].


%%Testcases
get_node_name_test() ->
  ?assertEqual(node_1, get_node_name("node_1.conf")),
  ?assertEqual(node_1, get_node_name("nodes/node_1.conf")),
  ?assertEqual(node_1, get_node_name("n/o/d/e/s/node_1.cfg")).

split_string_test() ->
  ?assertEqual({"ladida", "ladida"}, split_string("ladida,ladida", ",")),
  ?assertEqual({"ladida", "ladida"}, split_string("ladida,ladida\n", ",")),
  ?assertEqual({"ladida", "ladida"}, split_string("ladida##ladida", "##")),
  ?assertEqual({"ladida", "ladida"}, split_string("ladida##ladida\n", "##")),
  ?assertEqual({"ladidaladida", ""}, split_string("ladidaladida", ",")).

get_config_from_lines_test() ->
  Lines = ["1,node_0\n", "2, node_4\n"],
  Lines2 = ["1,node_0", "2, node_4"],
  ?assertEqual([{1, node_0}, {2, node_4}], get_config_from_lines(Lines)),
  ?assertEqual([{1, node_0}, {2, node_4}], get_config_from_lines(Lines2)).

get_edges_from_config_test() ->
  Config = [{1, node_1}, {2, node_4}],
  NodeName = node_0,

  Expected_edge_1 = #edge {
    node_1 = #node { name = node_0, pid = undefined },
    node_2 = #node { name = node_1, pid = undefined },
    weight = 1,
    type = basic
  },

  Expected_edge_2 = #edge {
    node_1 = #node { name = node_0, pid = undefined },
    node_2 = #node { name = node_4, pid = undefined },
    weight = 2,
    type = basic
  },
  ?assertEqual([Expected_edge_1, Expected_edge_2], get_edges_from_config(NodeName, Config)).
