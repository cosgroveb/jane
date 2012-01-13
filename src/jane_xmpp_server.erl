-module(jane_xmpp_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0, send_message/3, send_message/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_message(Body) ->
  send_message(list_to_binary(?USER_LOGIN), list_to_binary(?MUC_ROOM), Body).

send_message(From, To, Reply) ->
  gen_server:cast(jane_xmpp_server, {send_message, {From, To, Reply}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Session = connect(?USER_LOGIN, ?USER_PASSWORD, ?SERVER_DOMAIN),
  JoinedSession = join_room(Session, ?USER_LOGIN, ?MUC_ROOM),
  {ok, #state{session = JoinedSession}, 0}.

handle_info(Request, State) when ?IS_GROUP_MESSAGE(Request) ->
  Message = get_message(?MUC_ROOM, Request),
  jane_command_server:process_message(Message),
  {noreply, State};
handle_info(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_message, {From, To, Reply}}, State=#state{session=Session}) ->
  send_message(Session, From, To, Reply),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Private
%%%===================================================================

connect(Login, Password, Domain) ->
  application:start(exmpp),
  Session = exmpp_session:start({1,0}),
  [UserName, UserDomain] = string:tokens(Login,"@"),
  JID     = exmpp_jid:make(UserName, UserDomain, random),
  exmpp_session:auth_info(Session, JID, Password),
  exmpp_session:connect_TCP(Session, Domain, 5222),
  exmpp_session:login(Session, "PLAIN"),
  Session.

join_room(Session, Login, Room) ->
  Presence = exmpp_presence:presence(available, ""),
  Stanza = exmpp_xml:append_child(Presence, exmpp_xml:element("http://jabber.org/protocol/muc", x)),
  Packet = exmpp_xml:set_attributes(Stanza,[{<<"to">>, Room}, {<<"from">>, Login}]),
  exmpp_session:send_packet(Session, Packet),
  Session.

get_message(_, #received_packet{type_attr="error"}) ->
  {error};
get_message(Room, Request=#received_packet{raw_packet=Packet, type_attr="groupchat"}) ->

  SelfJID = exmpp_jid:parse(Room),
  {_,_,_,_,BotName} = SelfJID,
  Body = exmpp_message:get_body(Packet),
  To   = exmpp_xml:get_attribute(Packet, <<"from">>, "unknown"),
  From = exmpp_xml:get_attribute(Packet, <<"to">>, "unknown"),

  ShouldHandleMessage = (is_old_message(Request) == false) and
                        (is_from_self(Request, SelfJID) == false) and
                        has_botname(Body, BotName),

  if
    ShouldHandleMessage == true  -> {To, From, Body};
    ShouldHandleMessage == false -> {nomessage}
  end;
get_message(_, _Request) ->
  {error}.

send_message(Session, From, To, Message) ->
  [MucId|_Res] = string:tokens(binary_to_list(To), "/"),
  BodyXmlEl    = exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body), Message),
  MessageXmlEl = exmpp_xml:append_child(exmpp_xml:element("jabber:client", message), BodyXmlEl),
  PktWithAttrs = exmpp_xml:set_attributes(MessageXmlEl, [{<<"from">>, From}, {<<"to">>, MucId}, {<<"type">>, groupchat}]),
  exmpp_session:send_packet(Session, PktWithAttrs).

is_old_message(Request) ->
  exmpp_xml:has_element(Request#received_packet.raw_packet, x).

is_from_self(Request, SelfJID) ->
  SelfJID == exmpp_jid:make(Request#received_packet.from).

has_botname(undefined, _BotName) ->
  false;
has_botname(Body, BotName) ->
  string:rstr(string:to_lower(binary_to_list(Body)),string:to_lower(binary_to_list(BotName))) > 0.
