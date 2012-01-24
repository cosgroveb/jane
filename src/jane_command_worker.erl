-module(jane_command_worker).

-include_lib("jane.hrl").

-export([start_link/1, process_message/1, init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

start_link(Message) ->
  {_,{_,Body}} = Message,
  error_logger:info_msg("Starting jane_command_worker to handle command: ~p~n",[Body]),
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

handle_message({process_message, {From, Body}}) ->
  Sender = lists:nth(2,string:tokens(binary_to_list(From), "/")),
  Reply = case command:call(Sender, binary_to_list(Body)) of
    {error, _} ->
      error_logger:info_msg("Command not found: ~p~n", [binary_to_list(Body)]),
      "Sorry, I don't know what you mean.";
    {ok, Output} ->
      error_logger:info_msg("Command output: ~p~n", [Output]),
      Output
  end,
  jane_xmpp_server:send_message(Reply);
handle_message(_) ->
  {ok, nothing}.
