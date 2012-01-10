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
  application:start(exmpp),
  Session = xmpp_account:connect(?USER_LOGIN, ?USER_PASSWORD, ?SERVER_DOMAIN),
  JoinedSession = xmpp_account:join_room(Session, ?USER_LOGIN, ?MUC_ROOM),
  {ok, #state{session = JoinedSession}, 0}.

handle_info(Record, #state{session=Session}) when ?IS_GROUP_MESSAGE(Record) ->
  case xmpp_account:get_message(?MUC_ROOM, Record) of
    {To, From, Body} ->
      gen_server:cast(jane_command_server, {To, From, Body, Session});
      _ -> pass
  end,
  {noreply, #state{session=Session}};
handle_info(Record, Session=#state{session=Session}) when ?IS_PRESENCE(Record) ->
  xmpp_chat:handle_presence(Session, Record),
  {noreply, Session};
handle_info(_Record, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

