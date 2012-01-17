-module(jane_http).
-include_lib("jane.hrl").
-export([get/1, get/2, get_json/1]).

%%%===================================================================
%%% API
%%%===================================================================

get(Url) ->
  ibrowse:send_req(Url, [], get).

get(Url, Body) ->
  ibrowse:send_req(Url, [], get, Body).

get_json(Url) ->
  {ok, _StatusCode, _Headers, Body} = jane_http:get(Url),
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
  DeepDictify = fun(_K,V) ->
    case is_tuple(V) of
      true -> tuple_to_dict(V);
      _ -> V
    end
  end,

  dict:map(DeepDictify, Dict).
