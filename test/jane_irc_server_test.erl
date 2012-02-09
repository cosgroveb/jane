-module(jane_irc_server_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("jane.hrl").

is_chat_message_test() ->
  SplitInput = string:tokens(":cosgroveb!~cosgroveb@127.0.y.p PRIVMSG #test :jane hello\r\n", ": "),
  ?assert(jane_irc_server:is_chat_message(SplitInput) =:= true).

is_end_of_motd_test() ->
  SplitInput = string:tokens(":brians-macbook-pro.local 376 jane :End of /MOTD command.\r\n", ": "),
  ?assert(jane_irc_server:is_end_of_motd(SplitInput) =:= true).

is_ping_test() ->
  SplitInput = string:tokens("PING :brians-macbook-pro.local\r\n", ": "),
  ?assert(jane_irc_server:is_ping(SplitInput) =:= true).

parse_irc_packet_test() ->
  ChatPacket = ":cosgroveb!~cosgroveb@127.0.y.p PRIVMSG #test :jane hello\r\n",
  EndOfMOTDPacket = ":brians-macbook-pro.local 376 jane :End of /MOTD command.\r\n",
  PingPacket = "PING :brians-macbook-pro.local\r\n",
  ParsedChat = jane_irc_server:parse_irc_packet(ChatPacket),
  ParsedEndMOTD = jane_irc_server:parse_irc_packet(EndOfMOTDPacket),
  ParsedPing = jane_irc_server:parse_irc_packet(PingPacket),

  ?assert({irc_chat, "cosgroveb", "#test", "jane"} == ParsedChat),
  ?assert({irc_end_of_motd, null, null, null} == ParsedEndMOTD),
  ?assert({irc_ping, null, null, null} == ParsedPing).
