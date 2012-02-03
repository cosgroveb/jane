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
  XmppServer = ?CHILD(jane_xmpp_server, worker),
  CommandSupervisor = ?CHILD(jane_command_sup, supervisor),
  {ok, { {one_for_one, 1000, 60 * 60 * 1000}, [XmppServer, CommandSupervisor]} }.
