-module(web_request).
-include_lib("jane.hrl").
-export([get/1, get/2, get_json/1, get_json/2, tuple_to_dict/1, json_to_dict/1]).

%%%===================================================================
%%% API
%%%===================================================================

get(Url) ->
  ibrowse:send_req(Url, [], get).

get(Url, Body) ->
  ibrowse:send_req(Url, [], get, Body).

get_json(Url) ->
  {ok, _StatusCode, _Headers, Body} = ibrowse:send_req(Url, [{"Accept", "application/json"}], get),
  json_to_dict(Body).

get_json(Url, Options) ->
  {ok, _StatusCode, _Headers, Body} = ibrowse:send_req(Url, [{"Accept", "application/json"}], get, [], Options),
  json_to_dict(Body).

%% ===================================================================
%% Private
%% ===================================================================
%%
json_to_dict(Json) ->
  Output = jsonerl:decode(Json),
  tuple_to_dict(Output).

tuple_to_dict(Tuple) when is_tuple(Tuple) ->
  Dict = dict:from_list(tuple_to_list(Tuple)),
  dict:map(fun(_,V) -> tuple_to_dict(V) end, Dict);
tuple_to_dict([Head|List]) when is_list(List) ->
  [tuple_to_dict(Head)|tuple_to_dict(List)];
tuple_to_dict(Value) ->
  Value.
