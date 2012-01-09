-module(jane_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  application:start(exmpp),
  Session = xmpp_account:authenticate(?USER_LOGIN, ?USER_PASSWORD, ?SERVER_DOMAIN),
  JoinedSession = xmpp_account:join_room(Session, ?USER_LOGIN, ?MUC_ROOM),
  {ok, #state{session = JoinedSession}, 0}.

%% api callbacks

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(Record, #state{session=Session}) when ?IS_GROUP_MESSAGE(Record) ->
  case should_handle_message(Record) of
    true ->
      handle_message(Session,
       Record#received_packet.raw_packet,
       Record#received_packet.type_attr);
    false -> null
  end,
  {noreply, #state{session=Session}};
handle_info(Record, #state{session=Session}) when ?IS_PRESENCE(Record) ->
  handle_presence(Session,
    Record,
    Record#received_packet.raw_packet),
  {noreply, #state{session=Session}};
handle_info(_Record, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Helper for loop
should_handle_message(Record) ->
  SelfJID = exmpp_jid:parse(?MUC_ROOM),
  IsOldMessage = exmpp_xml:has_element(Record#received_packet.raw_packet, x),
  {_,_,_,_,BotName} = SelfJID,
  Packet = Record#received_packet.raw_packet,
  Body   = exmpp_message:get_body(Packet),
  IsFromSelf = SelfJID == exmpp_jid:make(Record#received_packet.from),
  HasBotName = string:rstr(string:to_lower(binary_to_list(Body)),string:to_lower(binary_to_list(BotName))) > 0,
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
          create_response(Body,[{sender,Resource}])),
        <<"from">>,
        From),
      <<"to">>,
      MucID),
    <<"type">>,
    TypeAttr),
  exmpp_session:send_packet(Session, SendPkt).

%% Handle bot commands here and return message body for XMPP reply
create_response(Body,MetaData) when is_binary(Body) and is_list(MetaData) ->
  case has_valid_command(?COMMANDS,binary_to_list(Body)) of
    false ->
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body), string:concat("I don't understand ", Body));
    {"", ReturnText} ->
      NewReturnText = [interpolate(X, MetaData) ++ " " || X <- string:tokens(ReturnText, " ")],
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body), NewReturnText);
    {Command, ReturnText} ->
      NewCommand = [interpolate(X, MetaData) ++ " " || X <- string:tokens(Command, " ")],
      os:cmd(NewCommand),
      NewReturnText = [interpolate(X, MetaData) ++ " " || X <- string:tokens(ReturnText, " ")],
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body), NewReturnText);
    Command ->
      NewCommand = [interpolate(X, MetaData) ++ " " || X <- string:tokens(Command, " ")],
      exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body), os:cmd(NewCommand))
  end.

interpolate([Sigil|Name],MetaData) when [Sigil] =:= "$" ->
  case get_metadata(list_to_atom(Name),MetaData) of
    false -> [Sigil] ++ Name;
    Value -> re:replace(Value,"[\\W]", "_", [global, {return, list}])
  end;
interpolate(Term,_) ->
  Term.

get_metadata(_Key,[{_Key, Value}|_]) ->
  Value;
get_metadata(_,[_|[]]) ->
  false;
get_metadata(Key,[_|Tail]) ->
  get_metadata(Key,Tail).

has_valid_command([], _) ->
  false;
has_valid_command([Vocabulary|Tail],Body) ->
  case string:str(Body,element(1, Vocabulary)) of
    0 -> has_valid_command(Tail, Body);
    _ -> command(Vocabulary)
  end.

command({_Word,Command}) ->
  Command;
command({_Word,Command,ReturnText}) ->
  {Command,ReturnText}.

handle_presence(Session, Packet, _Presence) ->
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

