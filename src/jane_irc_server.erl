-module(jane_irc_server).

-include_lib("jane.hrl").

-export([is_chat_message/1, is_end_of_motd/1, is_ping/1]).

is_chat_message([_User, "PRIVMSG", _Channel, _Login| _]) ->
  true;
is_chat_message(_Other) ->
  false.

is_end_of_motd([_, "376"|_]) ->
  true;
is_end_of_motd(_Other) ->
  false.

is_ping(["PING"| _T]) ->
  true;
is_ping(_IrcData) ->
  false.

parse_irc_packet(Packet) ->
  parse_split_input(string:tokens(Packet, ": ")).

parse_split_input([FullFromString, "PRIVMSG", Channel, Login| _]) ->
  From = lists:nth(1, string:tokens(FullFromString, "!")),
  {irc_chat, From, Channel, Login};
parse_split_input([_, "376"|_]) ->
  {irc_end_of_motd, null, null, null};
parse_split_input(["PING"| _T]) ->
  {irc_ping, null, null, null};
parse_split_input(_Other) ->
  {error}.
