-module(jane).
-export([start/0]).

start() ->
  application:start(jane).
