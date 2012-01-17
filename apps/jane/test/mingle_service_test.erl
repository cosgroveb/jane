-module(mingle_service_test).
-include_lib("eunit/include/eunit.hrl").

url_test() ->
  Url = mingle_service:get_url("mingle.example.com", "myproject", "1234"),
  ?assertEqual("https://mingle.example.com/projects/myproject/cards/1234", Url).

api_url_test() ->
  Url = mingle_service:get_api_url("mingle.example.com", "myproject", "1234"),
  ?assertEqual("https://mingle.example.com/api/v2/projects/myproject/cards/1234.xml", Url).

parse_xml_output_test() ->
  {Name, Description} = mingle_service:parse_xml_output({ok, {"card", [], [{"name", [], ["my name"]}, {"description", [], ["my description"]}]}, "\n"}),
  ?assertEqual("my name", Name),
  ?assertEqual("my description", Description).

