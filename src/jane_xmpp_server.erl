-module(jane_xmpp_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0, send_message/3, send_message/1, silence/0, unsilence/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session, silenced=false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  error_logger:info_msg("Starting jane_xmpp_server"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_message(Body) ->
  error_logger:info_msg("Sending message ~p~n", [Body]),
  send_message(list_to_binary(?app_env(user_login)), list_to_binary(?app_env(muc_room)), Body).

send_message(From, To, Reply) when is_list(Reply) ->
  error_logger:info_msg("Sending message ~n  From: ~p~n  To: ~p~n  Reply: ~p~n", [From, To, Reply]),
  gen_server:cast(jane_xmpp_server, {send_message, {From, To, Reply}}).

silence() ->
  send_message("Ok I won't talk until you tell me to start"),
  gen_server:cast(jane_xmpp_server, silence).

unsilence() ->
  gen_server:cast(jane_xmpp_server, unsilence).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Session = connect(?app_env(user_login), ?app_env(user_password), ?app_env(server_domain)),
  exmpp_session:send_packet(Session, join_room(?app_env(user_login), ?app_env(muc_room))),
  {ok, #state{session = Session, silenced = false}, 0}.

handle_info(Request, State) when ?IS_GROUP_MESSAGE(Request) ->
  Message = get_message(?app_env(muc_room), Request),
  jane_command_worker:process_message(Message),
  {noreply, State};
handle_info(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_message, {From, To, Reply}}, State=#state{session=Session, silenced=false}) ->
  Message = prepare_message(From, To, Reply),
  exmpp_session:send_packet(Session, Message),
  {noreply, State};
handle_cast(silence, State) ->
  {noreply, State#state{silenced=true}};
handle_cast(unsilence, State) ->
  {noreply, State#state{silenced=false}};
handle_cast(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Private
%%%===================================================================

connect(Login, Password, Domain) ->
  error_logger:info_msg("Connecting to xmpp server ~p as ~p~n", [Domain, Login]),
  application:start(exmpp),
  Session = exmpp_session:start({1,0}),
  [UserName, UserDomain] = string:tokens(Login,"@"),
  JID     = exmpp_jid:make(UserName, UserDomain, random),
  exmpp_session:auth_info(Session, JID, Password),
  error_logger:info_msg("Connecting on port ~p~n", [list_to_integer(?app_env(server_port))]),
  exmpp_session:connect_TCP(Session, Domain, list_to_integer(?app_env(server_port))),
  error_logger:info_msg("Logging in~n"),
  exmpp_session:login(Session, "PLAIN"),
  Session.

join_room(Login, Room) ->
  error_logger:info_msg("Joining xmpp room ~p as ~p~n", [Room, Login]),
  Presence = exmpp_presence:presence(available, ""),
  Stanza = exmpp_xml:append_child(Presence, exmpp_xml:element(?NS_MUC, x)),
  JoinRoomXml = exmpp_xml:set_attributes(Stanza,[{<<"to">>, Room}, {<<"from">>, Login}]),
  JoinRoomXml.

get_message(_, #received_packet{type_attr="error"}) ->
  {error};
get_message(Room, Request=#received_packet{raw_packet=Packet, type_attr="groupchat"}) ->
  SelfJID = exmpp_jid:parse(Room),
  {_,_,_,_,BotName} = SelfJID,
  Body = exmpp_message:get_body(Packet),
  From   = exmpp_xml:get_attribute(Packet, <<"from">>, "unknown"),

  ShouldHandleMessage = (is_old_message(Request) == false) and
                        (is_from_self(Request, SelfJID) == false) and
                        has_botname(Body, BotName),
  if
    ShouldHandleMessage == true  -> {From, Body};
    ShouldHandleMessage == false -> {nomessage}
  end;
get_message(_, _Request) ->
  {error}.

prepare_message(From, To, Message) ->
  [MucId|_Res] = string:tokens(binary_to_list(To), "/"),
  BodyXmlEl    = exmpp_xml:append_cdata(exmpp_xml:element(?NS_JABBER_CLIENT, body), Message),
  MessageXmlEl = exmpp_xml:append_child(exmpp_xml:element(?NS_JABBER_CLIENT, message), BodyXmlEl),
  MessageXml   = exmpp_xml:set_attributes(MessageXmlEl, [{<<"from">>, From}, {<<"to">>, MucId}, {<<"type">>, groupchat}]),
  MessageXml.

is_old_message(Request) ->
  exmpp_xml:has_element(Request#received_packet.raw_packet, x).

is_from_self(Request, SelfJID) ->
  SelfJID == exmpp_jid:make(Request#received_packet.from).

has_botname(undefined, _BotName) ->
  false;
has_botname(Body, BotName) ->
  string:rstr(string:to_lower(binary_to_list(Body)),string:to_lower(binary_to_list(BotName))) > 0.
