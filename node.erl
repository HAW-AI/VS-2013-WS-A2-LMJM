-module(node).
-export([loop/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datastructures.hrl").

loop(State) ->
  io:fread("", "~c").

log(Format, Data) ->
  io_lib:format("node: " ++ Format ++ "~n", Data).

%%Testcases
