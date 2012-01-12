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
  Sender = lists:nth(2,string:tokens(binary_to_list(To), "/")),
  Reply = case command:call(Sender, binary_to_list(Body)) of
    {error, _} -> "Sorry, I don't know what you mean.";
    {ok, Output} -> Output
  end,
  gen_server:cast(jane_chat_server, {send_message, {From, To, Reply}}),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_info(_Record, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
