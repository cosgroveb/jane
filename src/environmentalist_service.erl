-module(environmentalist_service).
-include("jane.hrl").
-export([index/0, reserve/2, release/1, show/1]).

%%%===================================================================
%%% API
%%%===================================================================

index() ->
  Url = string:concat(?app_env(environmentalist_url), "environments.json"),
  Body = web_request:get_json(Url),
  environments_to_string(
    environments_from_json(
      Body
    )
  ).

reserve(Reservee, EnvironmentName) ->
  case find_environment(EnvironmentName) of
    {Id, _Name, _ReservedBy, _UpdatedAt} ->
      Url = string:join([?app_env(environmentalist_url), "environments/", integer_to_list(Id), "/reserve.json"],""),
      FormData = string:concat("environment[reserved_by]=",Reservee),
      web_request:post_json(Url, FormData),
      reserved;
    false ->
      not_reserved
  end.

release(EnvironmentName) ->
  case find_environment(EnvironmentName) of
    {Id, _Name, _ReservedBy, _UpdatedAt} ->
      Url = string:join([?app_env(environmentalist_url), "environments/", integer_to_list(Id), "/release.json"],""),
      FormData = "",
      web_request:post_json(Url, FormData),
      released;
    false ->
      not_released
  end.

show(EnvironmentName) ->
  case find_environment(EnvironmentName) of
    {_Id, Name, ReservedBy, UpdatedAt} ->
      {ok, environment_to_string(Name, ReservedBy, UpdatedAt)};
    false ->
      not_found
  end.

%% ===================================================================
%% Private
%% ===================================================================

environments_from_json(WebRequestBody) ->
  lists:map(
    fun(E) ->
        {
          dict:fetch(<<"id">>,E),
          dict:fetch(<<"name">>,E),
          dict:fetch(<<"reserved_by">>,E),
          dict:fetch(<<"updated_at">>,E)
        }
    end, WebRequestBody).


environments_to_string(Environments) ->
  lists:map(
    fun (E) ->
        {_Id,Name,ReservedBy,Updated} = E,
        environment_to_string(Name, ReservedBy, Updated)
    end,
    Environments).

environment_to_string(Name, ReservedBy, Updated) ->
  case re:run(ReservedBy, "^$") of
    {match,_} ->
      string:concat(binary_to_list(Name)," is not reserved.\n");
    _ ->
      string:join(
        [
          binary_to_list(Name), "was reserved by",
          binary_to_list(ReservedBy), "at",
          binary_to_list(Updated),"\n"
        ]," ")
  end.

find_environment(EnvironmentName) ->
  Body = web_request:get_json(string:concat(?app_env(environmentalist_url),"environments.json")),
  Environments = environments_from_json(Body),
  lists:keyfind(list_to_binary(EnvironmentName), 2, Environments).

