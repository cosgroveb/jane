-module(jane_xmpp_server_test).
-include_lib("eunit/include/eunit.hrl").

join_room_test() ->
  error_logger:tty(false),
  JoinStanza = jane_xmpp_server:build_join_stanza("test@localhost","test@conference.localhost"),
  ?assert(exmpp_presence:is_presence(JoinStanza)          == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"to">>)   == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"from">>) == true).
