-module(jane_irc_server).

-include_lib("jane.hrl").

%%%===================================================================
%%% Private
%%%===================================================================

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

prepare_message(#message{room=Channel, from=_From, to=_To, body=Body}) ->
  "PRIVMSG " ++ Channel ++ " :" ++ Body ++ "\r\n".

connect(Login, Port, Domain) ->
  error_logger:info_msg("Connecting to irc server ~p as ~p~n", [Domain, Login]),
  case gen_tcp:connect(Domain, Port, [{packet, line}]) of
    {ok, Sock} ->
      gen_tcp:send(Sock, "NICK " ++ Login ++ "\r\n"),
      gen_tcp:send(Sock, "USER " ++ Login ++ " blah blah blah blah\r\n"),
      {ok, Sock};
    Error ->
      error_logger:info_msg("~njane_irc_server failed to connect~p",[Error]),
      exit({error, connection_failed})
  end.

disconnect(Socket) ->
  gen_tcp:close(Socket).
