-module(jane_irc_server_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("jane.hrl").

parse_irc_chat_packet_test() ->
  ChatPacket = ":cosgroveb!~cosgroveb@127.0.y.p PRIVMSG #test :jane hello\r\n",
  ParsedChat = jane_irc_server:parse_irc_packet(ChatPacket),
  ?assert({irc_chat, "cosgroveb", "#test", "jane"} == ParsedChat).

parse_irc_endmotd_packet_test() ->
  EndOfMOTDPacket = ":brians-macbook-pro.local 376 jane :End of /MOTD command.\r\n",
  ParsedEndMOTD = jane_irc_server:parse_irc_packet(EndOfMOTDPacket),
  ?assert({irc_end_of_motd, null, null, null} == ParsedEndMOTD).

parse_irc_ping_test() ->
  PingPacket = "PING :brians-macbook-pro.local\r\n",
  ParsedPing = jane_irc_server:parse_irc_packet(PingPacket),
  ?assert({irc_ping, null, null, null} == ParsedPing).

prepare_message_test() ->
  MessageFromCommandWorker = #message{
    room="#test",
    from=null,
    to  =null,
    body="Hello brian"
  },
  IrcPacketToSend = jane_irc_server:prepare_message(MessageFromCommandWorker),
  ExpectedOutput  = "PRIVMSG #test :Hello brian\r\n",

  ?assert(IrcPacketToSend == ExpectedOutput).

connect_bad_port_test() ->
  Login     = "jane_on_a_bad_port",
  BadPort   = 6578,
  Domain    = "127.0.0.1",
  ?assertExit({error, connection_failed}, jane_irc_server:connect(Login, BadPort, Domain)).

connect_test() ->
  _Pid   = spawn(fun irc_conn_faker/0),
  Login  = "jane",
  Port   = 5678,
  Domain = "127.0.0.1",
  {ok, Socket} = jane_irc_server:connect(Login, Port, Domain),

  ?assert(is_port(Socket)),

  jane_irc_server:disconnect(Socket).

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
