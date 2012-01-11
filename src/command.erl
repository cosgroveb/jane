-module(command).
-export([call/2]).

call(Sender, Input) when is_list(Input) ->
  call(string:tokens(Input, " "), Sender, Input);
call(Sender, Input) when is_binary(Input) ->
  call(Sender, binary_to_list(Input)).

call([], _Sender, _FullInput) ->
  {error, no_command};
call([Word|Input], Sender, FullInput) ->
  case command(Word, Sender, FullInput) of
    no_command -> call(Input, Sender, FullInput);
    Output     -> {ok, Output}
  end.

command("hello", Sender, _Input) -> string:concat("Hello ", Sender);
command("uptime", _Sender, _Input) -> os:cmd("uptime");
command("where are your bins?", _Input, _Sender) -> os:cmd("echo $PATH");
command("you suck", _Input, _Sender) -> "Oh :(";

command(_, _Sender, _Input) -> no_command.
