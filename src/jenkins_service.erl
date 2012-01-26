-module(jenkins_service).
-include("jane.hrl").
-export([failing_builds/0, box_status/0, build_info/1]).

%% ===================================================================
%% API
%% ===================================================================

failing_builds() ->
  Url = build_url(?app_env(jenkins_url)),
  Json = web_request:get_json(Url, [{basic_auth, {?app_env(jenkins_user), ?app_env(jenkins_password)}}]),
  parse_failing_builds(Json).

build_info(Build) ->
  Url = build_url(?app_env(jenkins_url), Build),
  Json = web_request:get_json(Url, [{basic_auth, {?app_env(jenkins_user), ?app_env(jenkins_password)}}]),
  parse_build_info(Json).

box_status() ->
  Url = build_box_status_url(?app_env(jenkins_url)),
  Json = web_request:get_json(Url, [{basic_auth, {?app_env(jenkins_user), ?app_env(jenkins_password)}}]),
  parse_box_status(Json).

%% ===================================================================
%% Private
%% ===================================================================

build_url(Domain) ->
  string:concat(Domain, "/api/json").
build_url(Domain, Build) ->
  build_url(string:join([Domain, "job", Build], "/")).

build_box_status_url(Domain) ->
  build_url(string:concat(Domain, "/computer")).

parse_failing_builds(Json) ->
  Builds = dict:fetch(<<"jobs">>, Json),
  FailingBuilds = lists:filter(fun(B) -> dict:fetch(<<"color">>, B) == <<"red">> end, Builds),
  lists:map(fun(B) -> binary_to_list(dict:fetch(<<"name">>, B)) end, FailingBuilds).

parse_build_info(Json) ->
  Status = case dict:fetch(<<"color">>, Json) of
    <<"blue">> -> "passing";
    _ -> "failing"
  end,

  [HealthReport|_] = dict:fetch(<<"healthReport">>, Json),
  HealthDescription = binary_to_list(dict:fetch(<<"description">>, HealthReport)),

  string:join(["Status: ", Status, ", ", HealthDescription], "").

parse_box_status(Json) ->
  BusyExecutors = integer_to_list(dict:fetch(<<"busyExecutors">>, Json)),
  TotalExecutors = integer_to_list(dict:fetch(<<"totalExecutors">>, Json)),
  Capacity = integer_to_list(round((list_to_integer(BusyExecutors) / list_to_integer(TotalExecutors)) * 100)),

  string:join(["Jenkins is at ", Capacity, "% capacity. ", BusyExecutors, "/", TotalExecutors, " executors are busy"], "").
