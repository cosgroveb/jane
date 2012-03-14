-module(jane_irc_server_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("jane.hrl").

parse_irc_chat_packet_test() ->
  ChatPacket = ":cosgroveb!~cosgroveb@127.0.y.p PRIVMSG #test :jane hello\r\n",
  ExpectedPacket = #irc_packet{
    type=irc_chat,
    channel="#test",
    from="cosgroveb",
    to  ="jane",
    body=string:concat("jane ",["hello"])
  },

  ParsedChat = jane_irc_server:parse_irc_packet(ChatPacket),

  ?assert(ExpectedPacket == ParsedChat).

parse_irc_endmotd_packet_test() ->
  EndOfMOTDPacket = ":testhost.local 376 jane :End of /MOTD command.\r\n",

  ParsedEndMOTD = jane_irc_server:parse_irc_packet(EndOfMOTDPacket),

  ?assert(#irc_packet{type=irc_end_of_motd, from=null, channel=null, to=null} == ParsedEndMOTD).

parse_irc_ping_test() ->
  PingPacket = "PING :testhost.local\r\n",
  ExpectedRecord = #irc_packet{
    type=irc_ping,
    from=null,
    channel=null,
    to=null,
    body=["testhost.local\r\n"]},
  ParsedPing = jane_irc_server:parse_irc_packet(PingPacket),

  ?assert(ExpectedRecord  == ParsedPing).

prepare_packet_test() ->
  MessageFromCommandWorker = #message{
    room="#test",
    from=null,
    to  =null,
    body="Hello brian"
  },
  ExpectedOutput  = "PRIVMSG #test :Hello brian\r\n",

  PreparedIrcPacket = jane_irc_server:prepare_packet(MessageFromCommandWorker),
  Result = ExpectedOutput == PreparedIrcPacket,
  ?assert(Result).

prepare_join_packet_test() ->
  error_logger:tty(false),

  Room = "#test",
  ExpectedPacket = "JOIN :#test\r\n",

  JoinRoomPacket = jane_irc_server:prepare_join_packet(Room),

  ?assert(ExpectedPacket == JoinRoomPacket).

prepare_ping_packet_test() ->
  Body = "anybody there?",
  ExpectedPacket = "PONG anybody there?\r\n",

  PingPacket = jane_irc_server:prepare_ping_packet(Body),

  ?assert(ExpectedPacket == PingPacket).

connect_bad_port_test() ->
  error_logger:tty(false),

  Login     = "jane_on_a_bad_port",
  BadPort   = "6578",
  Domain    = "127.0.0.1",

  ?assertExit(connection_failed, jane_irc_server:connect(Login, BadPort, Domain)).

connect_test() ->
  _Pid   = spawn(fun irc_conn_faker/0),
  Login  = "jane",
  Port   = "5678",
  Domain = "127.0.0.1",
  {ok, Socket} = jane_irc_server:connect(Login, Port, Domain),

  ?assert(is_port(Socket)).

irc_conn_faker() ->
  {ok, ListenSocket} = gen_tcp:listen(5678, [{active, false}]),
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  irc_conn_faker_loop(Socket).

irc_conn_faker_loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, _Data} ->
      % do nothing
      irc_conn_faker_loop(Socket);
    _Other -> ok
  end.
