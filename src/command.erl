-module(command).
-export([call/2]).
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST). -include("../test/command_test.hrl"). -endif.

call(Sender, Body) ->
  case find_and_run_command(Sender, Body, commands()) of
    error -> {error, no_command};
    Output -> {ok, Output}
  end.

find_and_run_command(_Sender, _Body, []) ->
  error;
find_and_run_command(Sender, Body, [{[], _CommandFun}|Commands]) ->
  find_and_run_command(Sender, Body, Commands);
find_and_run_command(Sender, Body, [{[CommandName|OtherCommandNames], CommandFun}|Commands]) ->
  case find_and_run_command(Sender, Body, [{CommandName, CommandFun}]) of
    error -> find_and_run_command(Sender, Body, [{OtherCommandNames, CommandFun}|Commands]);
    Output -> Output
  end;
find_and_run_command(Sender, Body, [{CommandName, CommandFun}|Commands]) ->
  case string:str(Body, binary_to_list(CommandName)) of
    0 -> find_and_run_command(Sender, Body, Commands);
    _ -> CommandFun(Sender, Body)
  end.

commands() ->
  [
    {[<<"hello">>, <<"hi">>], fun(Sender, _Body) ->
      string:concat("Hello ", Sender) end},

    {<<"uptime">>, fun(_Sender, _Body) ->
      os:cmd("uptime") end},

    {<<"where are your bins?">>, fun(_Sender, _Body) ->
      os:cmd("echo $PATH") end},

    {[<<"what is on sandbox?">>, <<"whats on sandbox">>, <<"sandbox revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://sandbox.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is on prod?">>, <<"whats on prod">>, <<"production revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://www.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is on qa?">>, <<"whats on qa">>, <<"qa revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://www.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is on qa2?">>, <<"whats on qa2">>, <<"qa2 revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://www.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is playing">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -H \"Accept: application/json\" http://jukebox2.local/playlist/current-track") end}
  ].
