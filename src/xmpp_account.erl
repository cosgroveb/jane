-module(xmpp_account).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([connect/3, join_room/3, get_message/2, send_message/4, handle_presence/2]).

connect(Login, Password, Domain) ->
  application:start(exmpp),
  Session = exmpp_session:start({1,0}),
  [UserName, UserDomain] = string:tokens(Login,"@"),
  JID     = exmpp_jid:make(UserName, UserDomain, random),
  Status  = exmpp_presence:set_status(exmpp_presence:unavailable(), ""),
  exmpp_session:auth_info(Session, JID, Password),
  exmpp_session:connect_TCP(Session, Domain, 5222),
  exmpp_session:login(Session, "PLAIN"),
  exmpp_session:send_packet(Session, Status),
  Session.

join_room(Session, Login, Room) ->
  exmpp_session:send_packet(
    Session,
    join_room_stanza(Room, Login,
      exmpp_presence:set_status(
        exmpp_presence:available(),
        ""))),
  Session.

join_room_stanza(Room, Login, Status) ->
  exmpp_xml:remove_element(
    exmpp_xml:set_attribute(
      exmpp_xml:set_attribute(
        exmpp_xml:append_child(
          Status,
          exmpp_xml:element(
            "http://jabber.org/protocol/muc",
            x)
        ),<<"to">>, Room),
      <<"from">>, Login),
    status).

handle_presence(Session, #received_packet{raw_packet=Packet}) ->
  case exmpp_jid:make(_From = Packet#received_packet.from) of
    JID ->
      case Type = Packet#received_packet.type_attr of
        "available" ->
          ok;
        "unavailable" ->
          ok;
        _ when (Type =:= "subscribe") or (Type =:= "subscribed") ->
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
