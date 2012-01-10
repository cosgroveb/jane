-module(jane_command_server).
-behavior(gen_server).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("jane.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  % application:start(exmpp),
  {ok, nothing, 0}.

%% api callbacks

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast({Session, RawPacket, TypeAttr}, State) ->
  handle_message(Session, RawPacket, TypeAttr),
  {noreply, State}.

handle_info(_Record, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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
          exmpp_xml:append_cdata(exmpp_xml:element("jabber:client", body), create_response(Body,[{sender,Resource}]))),
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
      string:concat("I don't understand ", Body);
    {"", ReturnText} ->
      _NewReturnText = [interpolate(X, MetaData) ++ " " || X <- string:tokens(ReturnText, " ")];
    {Command, ReturnText} ->
      NewCommand = [interpolate(X, MetaData) ++ " " || X <- string:tokens(Command, " ")],
      os:cmd(NewCommand),
      _NewReturnText = [interpolate(X, MetaData) ++ " " || X <- string:tokens(ReturnText, " ")];
    Command ->
      NewCommand = [interpolate(X, MetaData) ++ " " || X <- string:tokens(Command, " ")],
      os:cmd(NewCommand)
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
