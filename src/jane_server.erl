-module(jane_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Session = jane_xmpp:connect(?USER_LOGIN, ?USER_PASSWORD, ?SERVER_DOMAIN),
  JoinedSession = jane_xmpp:join_room(Session, ?USER_LOGIN, ?MUC_ROOM),
  {ok, #state{session = JoinedSession}, 0}.

handle_info(Request, State) when ?IS_GROUP_MESSAGE(Request) ->
  Message = jane_xmpp:get_message(?MUC_ROOM, Request),
  gen_server:cast(jane_command_server, {process_message, Message}),
  {noreply, State};
handle_info(Request, Session=#state{session=Session}) when ?IS_PRESENCE(Request) ->
  xmpp_chat:handle_presence(Session, Request),
  {noreply, Session};
handle_info(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_message, {From, To, Reply}}, State=#state{session=Session}) ->
  jane_xmpp:send_message(Session, From, To, Reply),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
