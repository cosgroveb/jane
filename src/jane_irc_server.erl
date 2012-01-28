-module(jane_irc_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0, send_message/1, silence/0, unsilence/0, join_room/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session, rooms=[], silenced=false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  error_logger:info_msg("Starting jane_irc_server"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_message(Message) ->
  error_logger:info_msg("Sending message ~n  From: ~p~n  To: ~p~n  Reply: ~p~n", [
    Message#message.from, Message#message.to, Message#message.body
  ]),
  gen_server:cast(jane_irc_server, {send_message, Message}).

silence() ->
  send_message("Ok I won't talk until you tell me to start"),
  gen_server:cast(jane_irc_server, silence).

unsilence() ->
  gen_server:cast(jane_irc_server, unsilence).

join_room(Room) ->
  gen_server:cast(jane_irc_server, {join_room, Room}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Session = connect(?app_env(irc_user_login), ?app_env(irc_port), ?app_env(irc_server_domain)),
  lists:foreach(fun(R) -> join_irc_channel(Session, R) end, ?app_env(irc_channels)),
  {ok, #state{session = Session, silenced = false, rooms=[?app_env(irc_channels)]}, 0}.

handle_info({tcp, _Socket, _Data}, State) ->
  %% Need to parse Data here and determine and respond to PING, PRIVMSG, 376 (end of MOTD)
  Message = "",
  jane_command_worker:process_message(Message),
  {noreply, State};
handle_info(quit, State) ->
  error_logger:info_message("jane_irc_server received quit message, quitting...~n"),
  gen_tcp:close(socket_goes_here),
  {noreply, State};
handle_info(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_message, Message}, State=#state{session=_Session, silenced=false}) ->
  _IrcMessage = prepare_message(Message),
  % send irc message %
  {noreply, State};
handle_cast(silence, State) ->
  {noreply, State#state{silenced=true}};
handle_cast(unsilence, State) ->
  {noreply, State#state{silenced=false}};
handle_cast({join_room, Room}, State) ->
  join_irc_channel(State#state.session, Room),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  error_logger:info_msg("jane_irc_server failed: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Private
%%%===================================================================

connect(Login, Port, Domain) ->
  error_logger:info_msg("Connecting to irc server ~p as ~p~n", [Domain, Login]),
  {ok, Sock} = gen_tcp:connect(Domain, Port, [{packet, line}]),
  gen_tcp:send(Sock, "NICK " ++ Login ++ "\r\n"),
  gen_tcp:send(Sock, "USER " ++ Login ++ "\r\n"),
  %% join server
  {ok, nothing}.

join_irc_channel(_Session, Room) ->
  error_logger:info_msg("Joining xmpp room ~p ~n", [Room]),
  %% join room
  {ok, nothing}.

prepare_message(#message{from=_From, to=_To, body=_Body}) ->
  "".
