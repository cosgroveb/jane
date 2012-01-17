join_room_test() ->
  JoinStanza = join_room("test@localhost","test@conference.localhost"),
  ?assert(exmpp_presence:is_presence(JoinStanza)          == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"to">>)   == true),
  ?assert(exmpp_xml:has_attribute(JoinStanza, <<"from">>) == true).

