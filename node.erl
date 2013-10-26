-module(node).
-export([loop/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("datastructures.hrl").

loop(State) ->
  receive
    {Message} ->
      log("~s", Message),
      loop(State)
  end.

log(Format, Data) ->
  io_lib:format("node: " ++ Format ++ "~n", Data).

%%Testcases
