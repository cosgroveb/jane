-module(jane_command_server).
-behavior(gen_server).

-include_lib("jane.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, nothing, 0}.

%% api callbacks

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({process_message, {To, From, Body}}, State) ->
  MetaData = [{sender, lists:nth(2,string:tokens(binary_to_list(To), "/"))}],
  Reply = case has_valid_command(?COMMANDS, binary_to_list(Body)) of
    false ->
      "Sorry, I don't know what you mean.";
    {"", ReturnText} ->
      [interpolate(X, MetaData) ++ " " || X <- string:tokens(ReturnText, " ")];
    {Command, ReturnText} ->
      NewCommand = [interpolate(X, MetaData) ++ " " || X <- string:tokens(Command, " ")],
      _ = os:cmd(NewCommand),
      [interpolate(X, MetaData) ++ " " || X <- string:tokens(ReturnText, " ")];
    Command ->
      os:cmd([interpolate(X, MetaData) ++ " " || X <- string:tokens(Command, " ")])
  end,
  gen_server:cast(jane_server, {send_message, {From, To, Reply}}),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info(_Record, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

has_valid_command([], _) ->
  false;
has_valid_command([Vocabulary|Tail],Body) ->
  case string:str(Body,element(1, Vocabulary)) of
    0 -> has_valid_command(Tail, Body);
    _ -> command(Vocabulary)
  end.

command({_Word,Command}) ->
  Command;
command({_Word,Command,ReturnText}) ->
  {Command,ReturnText}.

interpolate([Sigil|Name],MetaData) when [Sigil] =:= "$" ->
  case get_metadata(list_to_atom(Name),MetaData) of
    false -> [Sigil] ++ Name;
    Value -> re:replace(Value,"[\\W]", "_", [global, {return, list}])
  end;
interpolate(Term,_) ->
  Term.

get_metadata(_Key,[{_Key, Value}|_]) ->
  Value;
get_metadata(_,[_|[]]) ->
  false;
get_metadata(Key,[_|Tail]) ->
  get_metadata(Key,Tail).
