-module(jenkins_service_test).
-include_lib("eunit/include/eunit.hrl").

build_url_test() ->
  ExpctedOutput = "https://ci.example.com/api/json",
  Output = jenkins_service:build_url("https://ci.example.com"),
  ?assertEqual(ExpctedOutput, Output).

build_url_project_test() ->
  ExpctedOutput = "https://ci.example.com/job/example-build/api/json",
  Output = jenkins_service:build_url("https://ci.example.com", "example-build"),
  ?assertEqual(ExpctedOutput, Output).

parse_failing_builds_test() ->
  Struct = {{<<"jobs">>, [
        {{<<"name">>, <<"failing-build">>}, {<<"color">>, <<"red">>}},
        {{<<"name">>, <<"passing-build">>}, {<<"color">>, <<"green">>}}
      ]}},
  Output = jenkins_service:parse_failing_builds(web_request:tuple_to_dict(Struct)),
  ?assertEqual(["failing-build"], Output).

parse_build_info_test() ->
  Struct = {{<<"color">>, <<"blue">>}, {<<"healthReport">>, [{{<<"description">>, <<"Build stability: 4 out of the last 5 builds failed.">>}}]}},
  Output = jenkins_service:parse_build_info(web_request:tuple_to_dict(Struct)),
  ?assertEqual("Status: passing, Build stability: 4 out of the last 5 builds failed.", Output).

build_box_status_url_test() ->
  ExpctedOutput = "https://ci.example.com/computer/api/json",
  Output = jenkins_service:build_box_status_url("https://ci.example.com"),
  ?assertEqual(ExpctedOutput, Output).

parse_box_status_test() ->
  Struct = {{<<"totalExecutors">>, 9}, {<<"busyExecutors">>, 3}},
  Output = jenkins_service:parse_box_status(web_request:tuple_to_dict(Struct)),
  ?assertEqual("Jenkins is at 33% capacity. 3/9 executors are busy", Output).
