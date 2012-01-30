-module(jane_xmpp_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0, send_message/1, silence/0, unsilence/0, join_room/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {session, rooms=[], silenced=false}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  error_logger:info_msg("Starting jane_xmpp_server"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_message(Message) ->
  error_logger:info_msg("Sending message ~n  From: ~p~n  To: ~p~n  Reply: ~p~n", [
    Message#message.from, Message#message.to, Message#message.body
  ]),
  gen_server:cast(jane_xmpp_server, {send_message, Message}).

silence() ->
  send_message("Ok I won't talk until you tell me to start"),
  gen_server:cast(jane_xmpp_server, silence).

unsilence() ->
  gen_server:cast(jane_xmpp_server, unsilence).

join_room(Room) ->
  gen_server:cast(jane_xmpp_server, {join_room, Room}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Session = connect(?app_env(user_login), ?app_env(user_password), ?app_env(server_domain)),
  lists:foreach(fun(R) -> join_xmpp_room(Session, ?app_env(user_login), R) end, ?app_env(muc_rooms)),
  {ok, #state{session = Session, silenced = false, rooms=[?app_env(muc_rooms)]}, 0}.

handle_info(Request, State) when ?IS_GROUP_MESSAGE(Request) ->
  Message = parse_xmpp_message(Request),
  case should_handle_message(Request, Message) of
    true  -> jane_command_worker:process_message(Message);
    false -> nomessage
  end,
  {noreply, State};
handle_info(_Request, State) ->
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({send_message, Message}, State=#state{session=Session, silenced=false}) ->
  XmppMessage = prepare_message(Message),
  exmpp_session:send_packet(Session, XmppMessage),
  {noreply, State};
handle_cast(silence, State) ->
  {noreply, State#state{silenced=true}};
handle_cast(unsilence, State) ->
  {noreply, State#state{silenced=false}};
handle_cast({join_room, Room}, State) ->
  join_xmpp_room(State#state.session, ?app_env(user_login), Room),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  error_logger:info_msg("jane_xmpp_server failed: ~p~n", [Reason]),
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

join_xmpp_room(Session, Login, Room) ->
  error_logger:info_msg("Joining xmpp room ~p as ~p~n", [Room, Login]),
  JoinStanza = build_join_stanza(Login, Room),
  exmpp_session:send_packet(Session, JoinStanza).

build_join_stanza(Login, Room) ->
  Presence = exmpp_presence:presence(available, ""),
  Stanza = exmpp_xml:append_child(Presence, exmpp_xml:element(?NS_MUC, x)),
  exmpp_xml:set_attributes(Stanza,[{<<"to">>, Room}, {<<"from">>, Login}]).

should_handle_message(Request, Message) ->
  (is_old_message(Request) == false) and (is_from_self(Message#message.room, Request) == false).

parse_xmpp_message(#received_packet{raw_packet=Packet, type_attr="groupchat"}) ->
  Body = exmpp_message:get_body(Packet),
  From = exmpp_xml:get_attribute(Packet, <<"from">>, "unknown"),
  Bot = ?app_env(user_login),
  [RoomUrl|_]  = string:tokens(binary_to_list(From), "/"),
  [RoomUser|_] = string:tokens(Bot, "@"),

  #message{
    room = string:join([RoomUrl, RoomUser], "/"),
    to = Bot,
    from = From,
    body = Body,
    source = jane_xmpp_server
  };
parse_xmpp_message(_Request) ->
  {error}.

prepare_message(#message{from=From, to=To, body=Body}) ->
  [MucId|_Res] = string:tokens(binary_to_list(To), "/"),
  BodyXmlEl    = exmpp_xml:append_cdata(exmpp_xml:element(?NS_JABBER_CLIENT, body), Body),
  MessageXmlEl = exmpp_xml:append_child(exmpp_xml:element(?NS_JABBER_CLIENT, message), BodyXmlEl),
  MessageXml   = exmpp_xml:set_attributes(MessageXmlEl, [{<<"from">>, From}, {<<"to">>, MucId}, {<<"type">>, groupchat}]),
  MessageXml.

is_old_message(RawMessage) ->
  exmpp_xml:has_element(RawMessage#received_packet.raw_packet, x).

is_from_self(Room, Request) ->
  exmpp_jid:parse(Room) == exmpp_jid:make(Request#received_packet.from).

