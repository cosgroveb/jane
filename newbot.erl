-module(newbot).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, stop/1]).
-export([init/6]).

start() ->
    spawn(?MODULE, init, ["jane", "localhost", "password", "localhost", 5222, "test@conference.localhost"]).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

init(User, UserDomain, Password, ServerDomain, Port, Muc) ->
  application:start(exmpp),
  Session = exmpp_session:start({1,0}),
  JID = exmpp_jid:make(User, UserDomain, random),
  Status = exmpp_presence:set_status( exmpp_presence:available(), "online"),
  MucNS = "http://jabber.org/protocol/muc",
  MucTo = <<"to">>,
  MucFrom = <<"from">>,
  JoinRoom = exmpp_xml:set_attribute(exmpp_xml:set_attribute(exmpp_xml:append_child(Status,exmpp_xml:element(MucNS,x)),MucTo,Muc),MucFrom,"jane@localhost"),
  JoinRoom2 = exmpp_xml:remove_element(JoinRoom,status),
  exmpp_session:auth_info(Session, JID, Password),
  {ok, _StreamId, _Features} = exmpp_session:connect_TCP(Session, ServerDomain, Port),
  exmpp_session:login(Session, "PLAIN"),
  exmpp_session:send_packet(Session, Status),
  % io:format("~n~nJoin Data ~p~n~n",[JoinRoom]),
  % io:format("~n~nStatus Stanza ~p~n~n",[exmpp_stanza:to_list(Status)]),
  % io:format("~n~nJoin Stanza ~p~n~n",[exmpp_stanza:to_list(JoinRoom2)]),
  % try exmpp_session:send_packet(Session, JoinRoom2)
  % catch
  %   throw:X -> io:format("~nthrow ~p~n",[X]);
  %   exit:X  -> io:format("~nexit ~p~n",[X]);
  %   error:X -> io:format("~nerror ~p~n",[X]);
  %   _:_ -> erlang:display(erlang:get_stacktrace())
  % end,
  io:format("~n~nGot here~n~n",[]),
  loop(Session).

loop(MySession) ->
  receive
    stop ->
      exmpp_session:stop(MySession);
    _Record = #received_packet{packet_type=message, raw_packet=Packet, type_attr=_Type} ->
      io:format("Received Message stanza:~n~p~n~n", [_Record]),
      handle_packet(MySession, Packet),
      loop(MySession);
    Record when Record#received_packet.packet_type == 'presence' ->
      io:format("Received Presence stanza:~n~p~n~n", [Record]),
      handle_presence(MySession, Record, Record#received_packet.raw_packet),
      loop(MySession);
    _Record when _Record#received_packet.packet_type == 'iq' ->
      io:format("Received IQ stanza:~n~p~n~n", [_Record]),
      loop(MySession);
    _Record ->
      io:format("~n~nReceived other stanza:~n~p~n~n", [_Record]),
      loop(MySession)
    end.

%% Todo: replace temp variable stuff with recursive utility function
handle_packet(MySession, Packet) ->
  HasBody = exmpp_xml:has_element(Packet, "body"),
  if HasBody == true ->
      % io:format("~ngot here received packet~n~n ~p~nend packet~n",[Packet]),
      Body = exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, "body")),
      To   = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
      From = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
      % io:format("~ngot there~n",[]),
      BodyEl = if Body == <<"date">> ->
          Date = formatted_date(erlang:localtime()),
          exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),Date);
        Body =/= <<"date">> ->
          exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),string:concat( "I don't understand ",Body))
      end,
      % io:format("~n~nBODYEL ~p~n",[BodyEl]),
      NewMessageEl = exmpp_xml:element("jabber:client", message),
      % io:format("~n~nMSGEL ~p~n",[NewMessageEl]),
      TmpPacket = exmpp_xml:append_child(NewMessageEl, BodyEl),
      % io:format("~n~nTMPPKT ~n~p~n",[TmpPacket]),
      Attrs = [{"to",To},{"from",From}],
      % io:format("~n~nATTRS ~p~n",[Attrs]),
      % io:format("~nugh~n~p~n",[exmpp:set_attributes("jabber:client", Packet, Attrs)]),
      % {ok, NewPacket} = try exmpp:set_attributes(TmpPacket, Attrs) of
      %   Val -> io:format("~nVAL ~p~n",[Val])
      % catch
      %   throw:X -> io:format("~nthrow ~p~n",[X]);
      %   exit:X  -> io:format("~nexit ~p~n",[X]);
      %   error:X -> io:format("~nerror ~p~n",[X])
      % end,
      TempPkt1 = exmpp_xml:set_attribute(TmpPacket, <<"from">>, From),
      TempPkt2 = exmpp_xml:set_attribute(TempPkt1, <<"to">>, To),
      % io:format("~n~ndidnt get here? ~p~n~n",[NewPacket]);
      exmpp_session:send_packet(MySession, TempPkt2);
     HasBody == false -> false
end.

handle_presence(Session, Packet, _Presence) ->
  case exmpp_jid:make(_From = Packet#received_packet.from) of
	JID ->
    case _Type = Packet#received_packet.type_attr of
		"available" ->
      ok;
		"unavailable" ->
      ok;
		"subscribe" ->
      presence_subscribed(Session, JID),
      presence_subscribe(Session, JID);
		"subscribed" ->
      presence_subscribed(Session, JID),
      presence_subscribe(Session, JID)
    end
  end.

presence_subscribed(Session, Recipient) ->
  Presence_Subscribed = exmpp_presence:subscribed(),
  Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
  exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
  Presence_Subscribe = exmpp_presence:subscribe(),
  Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
  exmpp_session:send_packet(Session, Presence).

formatted_date(DateTime) ->
  {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
    [Year, Month, Day, Hour, Min, Sec]).

