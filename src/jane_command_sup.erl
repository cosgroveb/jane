-module(jane_command_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Message) ->
  supervisor:start_child(?SERVER, [Message]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Children = [{jane_command_worker,{jane_command_worker,start_link,[]},
                                    transient,brutal_kill,worker,
                                    [jane_command_worker]}],
  RestartStrategy = {simple_one_for_one, 10, 120},
  {ok, {RestartStrategy, Children}}.

