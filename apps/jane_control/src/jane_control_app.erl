%% @doc Callbacks for the jane_control application.

-module(jane_control_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for jane_control.
start(_Type, _StartArgs) ->
    jane_control_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for jane_control.
stop(_State) ->
    ok.
