-module(jane_command_worker).

-include_lib("jane.hrl").
-include_lib("command.hrl").

-export([start_link/1, process_message/1, init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

start_link(Message) ->
  error_logger:info_msg("Starting jane_command_worker to handle command: ~p~n",[Message]),
  proc_lib:start_link(?MODULE, init, [Message]).

process_message(Message) ->
  jane_command_sup:start_child({process_message,Message}),
  {ok, nothing, 0}.

init(Message) ->
  proc_lib:init_ack({ok, self()}),
  handle_message(Message).

%% ===================================================================
%% Private
%% ===================================================================

handle_message({process_message, #message{room=Room, to=To, from=From, body=Body}}) ->
  Sender = lists:nth(2,string:tokens(binary_to_list(From), "/")),
  Reply = case eval_message(command:commands(), Sender, binary_to_list(Body)) of
    error ->
      error_logger:info_msg("Command not found: ~p~n", [binary_to_list(Body)]),
      NotFoundResponses = [
        "Sorry, I don't know what you mean",
        "I have no idea what you're talking about",
        "Hmm? Maybe ask for help"
      ],
      random:seed(erlang:now()),
      lists:nth(random:uniform(length(NotFoundResponses)), NotFoundResponses);
    Output ->
      error_logger:info_msg("Command output: ~p~n", [Output]),
      Output
  end,
  jane_xmpp_server:send_message(#message{room=Room, to=From, from=To, body=Reply});
handle_message(_) ->
  {ok, nothing}.

eval_message([], _Sender, _Body) ->
  error;
eval_message([Command|Commands], Sender, Body) ->
  #command{matches=Matches, action=Action, subcommands=SubCommands} = Command,
  PaddedBody = string:join([" ", Body, " "], ""),
  PaddedMatches = string:join(["\s", Matches, "\s"], ""),

  case re:run(PaddedBody, PaddedMatches) of
    nomatch ->
      eval_message(Commands, Sender, Body);
    _ ->
      case eval_message(SubCommands, Sender, Body) of
        error -> call_action(Action, Sender, Body);
        Output -> Output
      end
  end.

call_action(Action, Sender, Body) when is_function(Action) ->
  Action(Sender, Body);
call_action(_, _, _) ->
  error.
