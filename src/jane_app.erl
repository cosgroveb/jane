-module(jane_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  application:start(inets),
  ibrowse:start(),

  error_logger:info_msg("Starting jane_app~n"),
  jane_sup:start_link().

stop(_State) ->
  ok.

