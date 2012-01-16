%% @doc jane_control webmachine_resource.

-module(jane_control_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
  io:format("~ngot here at least~n",[]),
  PathInfo = wrq:path_info(ReqData),
  io:format("~nhere too~n",[]),
  Content = case dict:find(key, PathInfo) of
    {ok, Action} -> io:format("~ngot here~n",[]),
      Action;
    Other -> io:format("~ngot here instead~n",[]),
      Other
  end,
  io:format("~nmade it all the way here ~p~n",[Content]),
  Html = string:concat(string:concat("<html><head></head><body>","nothing to see"),"</body></html>"),
  % {ok, Content} = index_dtl:render(),
  {Html, ReqData, State}.
