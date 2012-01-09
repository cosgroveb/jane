-module(xmpp_account).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-compile([export_all]).

authenticate(Login, Password, Domain) ->
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
