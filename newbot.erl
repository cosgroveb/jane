-module(erlbot).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, stop/1]).
-export([init/5]).

start() ->
    spawn(?MODULE, init, ["test", "localhost", "password", "ec2-107-22-120-36.compute-1.amazonaws.com", 5222]).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

init(User, UserDomain, Password, ServerDomain, Port) ->
  application:start(exmpp),
  Session = exmpp_session:start({1,0}),
  JID = exmpp_jid:make(User, UserDomain, random),
  Status = exmpp_presence:set_status( exmpp_presence:available(), "Commandbot"),
  exmpp_session:auth_info(Session, JID, Password),
  {ok, _StreamId, _Features} = exmpp_session:connect_TCP(Session, ServerDomain, Port),
  exmpp_session:login(Session, "PLAIN"),
  exmpp_session:send_packet(Session, Status),
  loop(Session).

loop(MySession) ->
  receive
    stop ->
      exmpp_session:stop(MySession);
    _Record = #received_packet{packet_type=message, raw_packet=Packet, type_attr=_Type} ->
        handle_packet(MySession, Packet),
        loop(MySession)
    end.

%% Todo: replace temp variable stuff with recursive utility function
handle_packet(MySession, Packet) ->
  HasBody = exmpp_xml:has_element(Packet, "body"),
  if HasBody == true ->
      io:format("~ngot here received packet~n~n ~p~nend packet~n",[Packet]),
      Body = exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, "body")),
      To   = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
      From = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
      io:format("~ngot there~n",[]),
      BodyEl = if Body == <<"date">> ->
          Date = formatted_date(erlang:localtime()),
          exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),Date);
        Body =/= <<"date">> ->
          exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),string:concat( "I don't understand ",Body))
      end,
      io:format("~n~nBODYEL ~p~n",[BodyEl]),
      NewMessageEl = exmpp_xml:element("jabber:client", message),
      io:format("~n~nMSGEL ~p~n",[NewMessageEl]),
      TmpPacket = exmpp_xml:append_child(NewMessageEl, BodyEl),
      io:format("~n~nTMPPKT ~n~p~n",[TmpPacket]),
      Attrs = [{"to",To},{"from",From}],
      io:format("~n~nATTRS ~p~n",[Attrs]),
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
      TempPkt3 = exmpp_xml:set_attributes(TempPkt2, [Attrs]),
      % io:format("~n~ndidnt get here? ~p~n~n",[NewPacket]);
      exmpp_session:send_packet(MySession, TempPkt3);
     HasBody == false -> false
end.

formatted_date(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

