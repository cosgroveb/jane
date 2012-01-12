-module(jane_xmpp).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([connect/3, join_room/3, get_message/2, send_message/4]).

connect(Login, Password, Domain) ->
  application:start(exmpp),
  Session = exmpp_session:start({1,0}),
  [UserName, UserDomain] = string:tokens(Login,"@"),
  JID     = exmpp_jid:make(UserName, UserDomain, random),
  exmpp_session:auth_info(Session, JID, Password),
  exmpp_session:connect_TCP(Session, Domain, 5222),
  exmpp_session:login(Session, "PLAIN"),
  Session.

join_room(Session, Login, Room) ->
  Presence = exmpp_presence:presence(available, ""),
  Stanza = exmpp_xml:append_child(Presence, exmpp_xml:element("http://jabber.org/protocol/muc", x)),
  Packet = exmpp_xml:set_attributes(Stanza,[{<<"to">>, Room}, {<<"from">>, Login}]),
  exmpp_session:send_packet(Session, Packet),
  Session.

get_message(_, #received_packet{type_attr="error"}) ->
    error;
get_message(Room, Request=#received_packet{raw_packet=Packet}) ->
  SelfJID = exmpp_jid:parse(Room),
  {_,_,_,_,BotName} = SelfJID,
  Body = exmpp_message:get_body(Packet),
  To   = exmpp_xml:get_attribute(Packet, <<"from">>, "unknown"),
  From = exmpp_xml:get_attribute(Packet, <<"to">>, "unknown"),

  ShouldHandleMessage = (is_old_message(Request) == false) and
                        (is_from_self(Request, SelfJID) == false) and
                        has_botname(Body, BotName),

  if
    ShouldHandleMessage == true  -> {To, From, Body};
    ShouldHandleMessage == false -> {nomessage}
  end.

send_message(Session, From, To, Message) ->
  [MucID|_Resource] = string:tokens(binary_to_list(To), "/"),
  XmppMessage = exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body), Message),

  SendPkt = exmpp_xml:set_attribute(
    exmpp_xml:set_attribute(
      exmpp_xml:set_attribute(
        exmpp_xml:append_child(
          exmpp_xml:element("jabber:client",
            message),
          XmppMessage),
        <<"from">>,
        From),
      <<"to">>,
      MucID),
    <<"type">>,
    groupchat),
  exmpp_session:send_packet(Session, SendPkt).

is_old_message(Request) ->
  exmpp_xml:has_element(Request#received_packet.raw_packet, x).

is_from_self(Request, SelfJID) ->
  SelfJID == exmpp_jid:make(Request#received_packet.from).

has_botname(Body, BotName) ->
  string:rstr(string:to_lower(binary_to_list(Body)),string:to_lower(binary_to_list(BotName))) > 0.
