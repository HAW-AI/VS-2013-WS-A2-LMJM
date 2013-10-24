-module(node).
-export([start/1]).

start(ConfigFile) ->
  Lines = read_lines(ConfigFile).

read_lines(File) ->
  {ok, IoDevice} = file:open(File, read),
  read_lines_recursion(file:read_line(IoDevice), IoDevice, []).

read_lines_recursion({ok, Line}, IoDevice, List) ->
  NewList = lists:append(List, [Line]),
  read_lines_recursion(file:read_line(IoDevice), IoDevice, NewList);
read_lines_recursion(eof, IoDevice, List) ->
  file:close(IoDevice),
  List.
