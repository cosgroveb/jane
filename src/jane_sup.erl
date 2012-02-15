-module(jane_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).


%% ===================================================================
%% API
%% ===================================================================

start_link() ->
  error_logger:info_msg("Starting jane_sup~n"),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  CommandSupervisor = ?CHILD(jane_command_sup, supervisor),
  XmppServers = get_chat_servers(xmpp),
  IrcServers = get_chat_servers(irc),

  Children = [CommandSupervisor] ++ IrcServers ++ XmppServers,
  {ok, { {one_for_one, 1000, 60 * 60 * 1000}, Children} }.

get_chat_servers(TypeAtom) ->
  Type = atom_to_list(TypeAtom),
  Server = list_to_atom(string:join(["jane", Type, "server"], "_")),
  User = list_to_atom(string:join([Type, "user", "login"], "_")),

  case application:get_env(jane, User) of
    undefined -> [];
    _ -> [?CHILD(Server, worker)]
  end.
