-module(jane_irc_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0, send_message/1, silence/0, unsilence/0, join_room/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session, room, silenced=false}).

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
  gen_server:cast(jane_irc_server, silence).

unsilence() ->
  gen_server:cast(jane_irc_server, unsilence).

join_room(Channel) ->
  gen_server:cast(jane_irc_server, {join_room, Channel}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  gen_server:cast(jane_irc_server, connect),
  {ok, #state{silenced=false}, 0}.

handle_info({tcp, Socket, Data}, State) ->
  BotName = ?app_env(irc_user_login),
  case parse_irc_packet(Data) of
    #irc_packet{type=irc_chat, from=From, channel=Channel, to=To, body=Body} when BotName == To ->
       Message = #message{
        room=Channel,
        to=BotName,
        from=list_to_binary("fake@conference.example.com/" ++ From),
        body=list_to_binary(Body),
        source=jane_irc_server},
       error_logger:info_msg("IRC: Processing message - ~p",[Message]),
       jane_command_worker:process_message(Message);
    #irc_packet{type=irc_end_of_motd} ->
      error_logger:info_msg("IRC: end of MOTD received"),
      join_room(?app_env(irc_channel));
    #irc_packet{type=irc_ping, body=Body} ->
      error_logger:info_msg("IRC: received ping"),
      ping(Socket, Body);
    Other ->
      error_logger:info_msg("IRC: ignoring packet ~p", [Other])
  end,
  {noreply, State};
handle_info(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(connect, _State) ->
  {ok, Session} = connect(?app_env(irc_user_login), ?app_env(irc_server_port), ?app_env(irc_server_domain)),
  error_logger:info_msg("jane_irc_server Session ~p~n",[Session]),
  {noreply, #state{session = Session, silenced = false, room=?app_env(irc_channel)}};
handle_cast({send_message, Message}, State=#state{session=Session, silenced=false}) ->
  IrcMessage = prepare_packet(Message),
  error_logger:info_msg("Sending ~n~p~n",[IrcMessage]),
  send_chat(Session, IrcMessage),
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

parse_irc_packet(Packet) ->
  parse_split_input(string:tokens(Packet, ": ")).

parse_split_input([FullFromString, "PRIVMSG", Channel, To| Rest]) ->
  [Str|_] = Rest,
  Body = string:concat(To, string:concat(" ",string:tokens(Str, "\r\n"))),
  From = lists:nth(1, string:tokens(FullFromString, "!")),
  irc_packet_record(irc_chat, From, Channel, To, Body);
parse_split_input([_, "376"|_]) ->
  irc_packet_record(irc_end_of_motd, null, null, null, null);
parse_split_input(["PING"| Rest]) ->
  irc_packet_record(irc_ping, null, null, null, Rest);
parse_split_input(Other) ->
  Other.

irc_packet_record(Type, From, Channel, To, Body) ->
  #irc_packet{
    type=Type,
    from=From,
    channel=Channel,
    to=To,
    body=Body}.

prepare_packet(#message{room=Channel, from=_From, to=_To, body=Body}) ->
  "PRIVMSG " ++ Channel ++ " :" ++ Body ++ "\r\n".

prepare_join_packet(Room) ->
  "JOIN :" ++ Room ++ "\r\n".

prepare_ping_packet(Body) ->
  "PONG " ++ Body ++ "\r\n".

connect(Login, Port, Domain) ->
  error_logger:info_msg("Connecting to irc server ~p as ~p~n", [Domain, Login]),
  case gen_tcp:connect("brians-macbook-pro.local", list_to_integer(Port), [{packet, line}]) of
    {ok, Sock} ->
      gen_tcp:send(Sock, "NICK " ++ Login ++ "\r\n"),
      gen_tcp:send(Sock, "USER " ++ Login ++ " additional user info here\r\n"),
      {ok, Sock};
    Error ->
      error_logger:info_msg("~njane_irc_server failed to connect~p",[Error]),
      exit(connection_failed)
  end.

join_irc_channel(Socket, Room) ->
  JoinPacket = prepare_join_packet(Room),
  gen_tcp:send(Socket, JoinPacket).

ping(Socket, Body) ->
  PingPacket = prepare_ping_packet(Body),
  gen_tcp:send(Socket, PingPacket).

send_chat(Socket, Body) ->
  gen_tcp:send(Socket, Body).

