-module(command_test).
-include_lib("eunit/include/eunit.hrl").

command_alias_test() ->
  Output = "Hello foo",
  {ok, Output} = command:call("foo", "hello"),
  {ok, Output} = command:call("foo", "hi").

hello_test() ->
  {ok, Output} = command:call("foo", "hello"),
  Output = "Hello foo".

uptime_test() ->
  {ok, Output} = command:call("foo", "uptime"),
  ?assert(string:str(Output, "users, load averages") > 0).
