-module(jane_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0, join_chat/0, setup_and_join/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  application:start(exmpp),
  {ok, []}.

join_chat() ->
  gen_server:call(?MODULE, join_chat).

%% api callbacks

handle_call(join_chat, _From, _State) ->
  Pid = spawn(fun jane_server:setup_and_join/0),
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

setup_and_join() ->
  Session = exmpp_session:start({1,0}),
  [UserName, UserDomain] = string:tokens(?USER_LOGIN,"@"),
  JID     = exmpp_jid:make(UserName, UserDomain, random),
  Status  = exmpp_presence:set_status(exmpp_presence:unavailable(), ""),
  exmpp_session:auth_info(Session, JID, ?USER_PASSWORD),
  exmpp_session:connect_TCP(Session, ?SERVER_DOMAIN, 5222),
  exmpp_session:login(Session, "PLAIN"),
  exmpp_session:send_packet(Session, Status),
  exmpp_session:send_packet(
    Session,
    join_room_stanza(
      exmpp_presence:set_status(
        exmpp_presence:available(),
        ""))),
  loop(Session).

join_room_stanza(Status) ->
  exmpp_xml:remove_element(
    exmpp_xml:set_attribute(
      exmpp_xml:set_attribute(
        exmpp_xml:append_child(
          Status,
          exmpp_xml:element(
            "http://jabber.org/protocol/muc",
            x)
        ),<<"to">>,?MUC_ROOM),
      <<"from">>,?USER_LOGIN),
    status).

%% Server-to-client messages in jabber:client can be of three types:
%% message, presence, and iq.
loop(Session) ->
  receive
    stop ->
      exmpp_session:stop(Session);
    Record when ?IS_GROUP_MESSAGE(Record) ->
      case should_handle_message(Record) of
        true ->
          handle_message(Session,
           Record#received_packet.raw_packet,
           Record#received_packet.type_attr);
        false -> null
      end,
      loop(Session);
    Record when ?IS_PRESENCE(Record) ->
      handle_presence(Session,
        Record,
        Record#received_packet.raw_packet),
      loop(Session);
    _Record ->
      loop(Session)
    end.

%% Helper for loop
should_handle_message(Record) ->
  IsOldMessage = exmpp_xml:has_element(Record#received_packet.raw_packet, x),
  SelfJID = exmpp_jid:parse(?MUC_ROOM),
  {_,_,_,_,BotName} = SelfJID,
  Packet = Record#received_packet.raw_packet,
  Body   = exmpp_message:get_body(Packet),
  IsFromSelf = SelfJID == exmpp_jid:make(Record#received_packet.from),
  HasBotName = string:rstr(binary_to_list(Body),binary_to_list(BotName)) > 0,
  (IsFromSelf == false) and
  (HasBotName == true) and
  (IsOldMessage == false).

%% Handle messages
handle_message(_Session, Packet, _TypeAttr="error") ->
  io:format("~nError: ~p~n",[Packet]);
handle_message(Session, Packet, TypeAttr) ->
  Body = exmpp_message:get_body(Packet),
  To   = exmpp_xml:get_attribute(Packet, <<"from">>, "unknown"),
  From = exmpp_xml:get_attribute(Packet, <<"to">>, "unknown"),
  [MucID|Resource] = string:tokens(binary_to_list(To), "/"),
  SendPkt = exmpp_xml:set_attribute(
    exmpp_xml:set_attribute(
      exmpp_xml:set_attribute(
        exmpp_xml:append_child(
          exmpp_xml:element("jabber:client",
            message),
          create_response(Body,[{name,Resource}])),
        <<"from">>,
        From),
      <<"to">>,
      MucID),
    <<"type">>,
    TypeAttr),
  exmpp_session:send_packet(Session, SendPkt).

%% Handle bot commands here and return message body for XMPP reply
create_response(Body,MetaData) when is_binary(Body) and is_list(MetaData) ->
  io:format("~nMeta name: ~p~n",[get_metadata(name, MetaData)]),
  case has_valid_command(?COMMANDS,binary_to_list(Body)) of
    false ->
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),string:concat( "I don't understand ",Body));
    Command ->
      Result = os:cmd(Command),
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body),Result)
  end.

get_metadata(_Key,[{_Key, Value}|_]) ->
  Value;
get_metadata(_Key,[_|[]]) ->
  false;
get_metadata(Key,[_|Tail]) ->
  get_metadata(Key,Tail).

has_valid_command([{CmdStr,CmdFull}|[]],Body)
    when is_list(CmdStr) andalso is_list(CmdFull) ->
  case string:rstr(Body,CmdStr) > 0 of
    false -> false;
    true -> CmdFull
  end;
has_valid_command([{CmdStr,CmdFull}|T],Body)
    when is_list(CmdStr) andalso is_list(CmdFull) ->
  case string:rstr(Body,CmdStr) > 0 of
    false -> has_valid_command(T,Body);
    true  -> CmdFull
  end;
has_valid_command(CommandList, Body) ->
  throw({jane_invalid_command_list,
        {command_list, CommandList},
        {message_body, Body}}).

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

