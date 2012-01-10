-module(commands).
-export([call_command/1]).

call_command(Input) ->
  Commands = ["hello", "uptime", "where are your bins?"],
  call_command(Input, Commands).

call_command(_Input, []) ->
  {error, no_command};
call_command(Input, [Command|Commands]) ->
  case string:str(Input, Command) of
    0 -> call_command(Input, Commands);
    _ -> {ok, command(Command)}
  end.

command("hello")                -> "Hello!";
command("uptime")               -> os:cmd("uptime");
command("where are your bins?") -> os:cmd("echo $PATH").
