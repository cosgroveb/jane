-module(newbot_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

% -define(USER_NAME,     element(2,newbot_app:get_env(user))).
% -define(USER_DOMAIN,   element(2,newbot_app:get_env(user_domain))).
% -define(USER_PASSWORD, element(2,newbot_app:get_env(user_password))).
% -define(SERVER_DOMAIN, element(2,newbot_app:get_env(server_domain))).
% -define(SERVER_PORT,   element(2,newbot_app:get_env(server_port))).
% -define(MUC_ROOM,      element(2,newbot_app:get_env(muc_room))).
-define(USER_NAME,     "jane").
-define(USER_DOMAIN,   "localhost").
-define(USER_PASSWORD, "password").
-define(SERVER_DOMAIN, "conference.localhost").
-define(SERVER_PORT,   5222).
-define(MUC_ROOM,      "test@conference.localhost/jane").

-export([start_link/0, join_chat/0, go/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).
-record(state, {port, lsock, request_count = 0}).

start_link() ->
      gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
      {ok, []}.

join_chat() ->
  gen_server:call(?MODULE, join_chat).

%% callbacks
handle_call(join_chat, _From, _State) ->
    Pid = spawn(fun newbot_server:go/0),
    {reply, joining, Pid};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

go() ->
  UserName = "jane",
  UserDomain = "localhost",
  UserPassword = "password",
  ServerDomain = "localhost",
  ServerPort = 5222,
  MucRoom = "test@conference.localhost/jane",
  application:start(exmpp),
  application:start(newbot_app),
  Session = exmpp_session:start({1,0}),
  JID = exmpp_jid:make(UserName, UserDomain, random),
  Status = exmpp_presence:set_status( exmpp_presence:available(), "online"),
  MucNS = "http://jabber.org/protocol/muc",
  MucTo = <<"to">>,
  MucFrom = <<"from">>,
  JoinRoom = exmpp_xml:set_attribute(exmpp_xml:set_attribute(exmpp_xml:append_child(Status,exmpp_xml:element(MucNS,x)),MucTo,MucRoom),MucFrom,"jane@localhost"),
  JoinRoom2 = exmpp_xml:remove_element(JoinRoom,status),
  exmpp_session:auth_info(Session, JID, UserPassword),
  erlang:display("got here"),
  erlang:display(["username: " ++ UserName ++ "user domain: " ++ UserDomain ++ "pass: " ++ UserPassword ++ "server domain: " ++ ServerDomain ++ "server port: " ++ ServerPort]),
  {ok, _StreamId, _Features} = exmpp_session:connect_TCP(Session, ServerDomain, ServerPort),
  erlang:display("connected"),
  exmpp_session:login(Session, "PLAIN"),
  exmpp_session:send_packet(Session, Status),
  exmpp_session:send_packet(Session, JoinRoom2),
  erlang:display("joined"),
  loop(Session).

loop(MySession) ->
  receive
    stop ->
      exmpp_session:stop(MySession);
    _Record = #received_packet{packet_type=message, raw_packet=Packet, type_attr=Type}  when Type =/= "error" ->
      % io:format("Received Message stanza:~n~p~n~n", [_Record]),
      handle_packet(MySession, Packet, Type),
      loop(MySession);
    Record when Record#received_packet.packet_type == 'presence' ->
      io:format("Received Presence stanza:~n~p~n~n", [Record]),
      handle_presence(MySession, Record, Record#received_packet.raw_packet),
      loop(MySession);
    _Record when _Record#received_packet.packet_type == 'iq' ->
      io:format("Received IQ stanza:~n~p~n~n", [_Record]),
      loop(MySession);
    _Record ->
      io:format("~n~nReceived other stanza:~n~p~n~n", [_Record]),
      loop(MySession)
    end.

%% Todo: replace temp variable stuff with recursive utility function
handle_packet(MySession, Packet, Type) ->
  HasBody = exmpp_xml:has_element(Packet, "body"),
  if
    HasBody == true ->
      Body = exmpp_xml:get_cdata(exmpp_xml:get_element(Packet, "body")),
      To   = exmpp_xml:get_attribute(Packet, <<"from">>, "unknown"),
      From = exmpp_xml:get_attribute(Packet, <<"to">>, "unknown"),
      FromStr = binary_to_list(To),
      BodyEl = create_response(Body),
      NewMessageEl = exmpp_xml:element("jabber:client", message),
      TmpPacket = exmpp_xml:append_child(NewMessageEl, BodyEl),
      TmpPkt1 = exmpp_xml:set_attribute(TmpPacket, <<"from">>, From),
      SendPkt = if
        (Type == "groupchat") and (FromStr =/= "test@conference.localhost/jane") ->
          io:format("~n~nIS A GROUP CHAT~n~n~p~n~n",[To]),
          [MucID|_Resource] = string:tokens(binary_to_list(To), "/"),
          exmpp_xml:set_attribute(exmpp_xml:set_attribute(TmpPkt1, <<"to">>, MucID), <<"type">>, "groupchat");
        true ->
          exmpp_xml:set_attribute(TmpPkt1, <<"to">>, To)
      end,
      if
        FromStr =/= "test@conference.localhost/jane" ->
          io:format("~n~nsending message~n~n",[]),
          exmpp_session:send_packet(MySession, SendPkt),
          true;
        true ->
          false
      end;
    true -> null
end.

create_response(Body) ->
  if
    Body == <<"date">> ->
      Date = formatted_date(erlang:localtime()),
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),Date);
    true ->
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),string:concat( "I don't understand ",Body))
end.


handle_presence(Session, Packet, _Presence) ->
  case exmpp_jid:make(_From = Packet#received_packet.from) of
	JID ->
    case _Type = Packet#received_packet.type_attr of
		"available" ->
      ok;
		"unavailable" ->
      ok;
		"subscribe" ->
      presence_subscribed(Session, JID),
      presence_subscribe(Session, JID);
		"subscribed" ->
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

formatted_date(DateTime) ->
  {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
    [Year, Month, Day, Hour, Min, Sec]).

