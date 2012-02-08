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
