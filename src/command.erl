-module(command).
-export([call/2]).

call(Sender, Body) ->
  case find_and_run_command(Sender, Body, commands()) of
    error -> {error, no_command};
    Output -> {ok, Output}
  end.

find_and_run_command(_Sender, _Body, []) ->
  error;
find_and_run_command(Sender, Body, [{CommandName, CommandFun}|Commands]) ->
  case string:str(Body, CommandName) of
    0 -> find_and_run_command(Sender, Body, Commands);
    _ -> CommandFun(Sender, Body)
  end.

commands() ->
  [
    {"hello", fun(Sender, _Body) ->
      string:concat("Hello ", Sender) end},

    {"uptime", fun(_Sender, _Body) ->
      os:cmd("uptime") end},

    {"where are your bins?", fun(_Sender, _Body) ->
      os:cmd("echo $PATH") end}
  ].
