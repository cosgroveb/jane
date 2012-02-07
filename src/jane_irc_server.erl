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
  % send_message("Ok I won't talk until you tell me to start"),
  gen_server:cast(jane_irc_server, silence).

unsilence() ->
  gen_server:cast(jane_irc_server, unsilence).

join_room(Room) ->
  gen_server:cast(jane_irc_server, {join_room, Room}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Session = connect(?app_env(irc_user_login), ?app_env(irc_server_port), ?app_env(irc_server_domain)),
  io:format("~nSession ~p~n",[Session]),
  % lists:foreach(fun(R) -> join_irc_channel(Session, R) end, ?app_env(irc_channels)),
  {ok, #state{session = Session, silenced = false, rooms=[?app_env(irc_channels)]}, 0}.

handle_info({tcp, Socket, Data}, State) ->
  %% Need to parse Data here and determine and respond to PING, PRIVMSG, 376 (end of MOTD)
  Login = ?app_env(irc_user_login),
  case string:tokens(Data, ": ") of
    [User, "PRIVMSG", Channel, Login| _] ->
      From = lists:nth(1, string:tokens(User, "!")),
      Body = "hello jane",
      Message = #message{room=Channel, to=?app_env(irc_user_login), from=list_to_binary(From), body=list_to_binary(Body), source=jane_irc_server},
      error_logger:info_msg("Processing message: ~p~n", [Message]),
      jane_command_worker:process_message(Message);
    [_, "376"|_] ->
      error_logger:info_msg("Joining rooms: ~p~n", [State#state.rooms]),
      lists:foreach(fun(R) -> join_irc_channel(Socket, R) end, State#state.rooms);
    ["PING"| T] ->
      gen_tcp:send(Socket, "PONG " ++ T ++ "\r\n");
    Other ->
      io:format("~ngot here...~n~p",[Other]),
      ok
  end,
  {noreply, State};
handle_info(quit, State) ->
  error_logger:info_msg("jane_irc_server received quit message, quitting...~n"),
  % gen_tcp:close(socket_goes_here),
  {noreply, State};
handle_info(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_message, Message}, State=#state{session=Session, silenced=false}) ->
  IrcMessage = prepare_message(Message),
  gen_tcp:send(Session, IrcMessage),
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
  {ok, Sock} = gen_tcp:connect("brians-macbook-pro.local", 6665, [{packet, line}]),
  io:format("~nGot here - Res: ~p~n",[Sock]),
  gen_tcp:send(Sock, "NICK " ++ Login ++ "\r\n"),
  gen_tcp:send(Sock, "USER " ++ Login ++ " blah blah blah blah\r\n"),
  Sock.

join_irc_channel(Session, Room) ->
  error_logger:info_msg("Joining IRC channel ~p ~n", [Room]),
  gen_tcp:send(Session, "JOIN :" ++ Room ++ "\r\n"),
  {ok, nothing}.
prepare_message(#message{room=Channel, from=_From, to=_To, body=Body}) ->
  "PRIVMSG " ++ Channel ++ " :" ++ Body ++ "\r\n".
