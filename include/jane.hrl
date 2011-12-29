% Macros to get settings
-define(USER_LOGIN,    element(2,application:get_env(jane, user_login))).
-define(USER_PASSWORD, element(2,application:get_env(jane, user_password))).
-define(SERVER_DOMAIN, element(2,application:get_env(jane, server_domain))).
-define(MUC_ROOM,      element(2,application:get_env(jane, muc_room))).
-define(COMMANDS,      element(2,application:get_env(jane, commands))).

% Guard expression to test a groupchat message
-define(IS_GROUP_MESSAGE(Record), (
  Record#received_packet.packet_type == 'message' andalso
  Record#received_packet.type_attr == "groupchat"
)).
