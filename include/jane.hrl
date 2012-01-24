-record(message, {room, to, from, body, raw_message}).

% Macros to get settings
-define(app_env(Key), (
  element(2, application:get_env(jane, Key))
)).

% Guard expression to test a groupchat message
-define(IS_GROUP_MESSAGE(Record), (
  Record#received_packet.packet_type == 'message' andalso
  Record#received_packet.type_attr == "groupchat"
)).

-ifdef(TEST).
-compile([export_all]).
-endif.
