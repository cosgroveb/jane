-module(command).
-export([call/2]).

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
      os:cmd("echo $PATH") end}
  ].
