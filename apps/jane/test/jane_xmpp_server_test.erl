-module(jane_xmpp_server_test).
-include_lib("eunit/include/eunit.hrl").

join_room_test() ->
  JoinStanza = jane_xmpp_server:join_room("test@localhost","test@conference.localhost"),
  ?assert(exmpp_presence:is_presence(JoinStanza)          == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"to">>)   == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"from">>) == true).

