-module(jane_xmpp_server_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

join_room_test() ->
  error_logger:tty(false),
  JoinStanza = jane_xmpp_server:build_join_stanza("test@localhost","test@conference.localhost"),
  ?assert(exmpp_presence:is_presence(JoinStanza)          == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"to">>)   == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"from">>) == true).

is_from_self_test() ->
  application:start(exmpp),
  RawMessage = #received_packet{from={<<"test">>,<<"conference.localhost">>,<<"jane">>}},
  Room = "test@conference.localhost/jane",
  ?assert(jane_xmpp_server:is_from_self(Room, RawMessage) == true).


is_from_self_fail_test() ->
  application:start(exmpp),
  RawMessage = #received_packet{from={<<"test">>,<<"conference.localhost">>,<<"bob">>}},
  Room = "test@conference.localhost/jane",
  ?assert(jane_xmpp_server:is_from_self(Room, RawMessage) == false).

