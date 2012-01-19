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

find_and_run_command_test() ->
  Output = command:find_and_run_command("foo", "hello", command:commands()),
  ?assert(Output == "Hello foo").

current_song_test() ->
  meck:new(web_request),
  meck:expect(web_request, get_json, fun(_) -> dict:from_list([{<<"title">>, <<"Foo">>}, {<<"artist">>, <<"Bar">>}]) end),
  {ok, Output} = command:call("foo", "whats playing"),
  ?assertEqual(Output, "Foo by Bar"),
  meck:unload(web_request).

is_it_raining_in_test() ->
  meck:new(web_request),
  meck:expect(web_request, get, fun(_) -> {ok, x, x, "<h1>No</h1><h2>Conditions for <strong>Chicago, Illinois, United States</strong><br/>on Mon, 16 Jan 2012 7:10 pm CST: <strong>Cloudy</strong> (7&deg;C, 43&deg;F)</h2>"} end),

  {ok, Output} = command:call("foo", "is it raining in chicago"),
  ?assertEqual(Output, "Conditions for Chicago, Illinois, United Stateson Mon, 16 Jan 2012 7:10 pm CST: Cloudy (7°C, 43°F)"),

  meck:unload(web_request).

get_command_list_test() ->
  Commands = [ {<<"command">>, command_fun}, {[<<"other command">>, <<"other">>], command_fun} ],
  ExpectedOutput = ["command", "other command, other"],
  Output = command:get_command_list(Commands),

  ?assertEqual(ExpectedOutput, Output).
