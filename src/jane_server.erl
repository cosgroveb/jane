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
      case has_valid_command(?COMMANDS, binary_to_list(Body)) of
        false ->
          xmpp_account:send_message(Session, From, To, <<"Sorry, I don't know what you mean.">>);
        Command ->
          xmpp_account:send_message(Session, From, To, os:cmd(Command))
      end;
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

% Private Functions
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
