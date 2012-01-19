-module(web_request_test).
-include_lib("eunit/include/eunit.hrl").

json_to_dict_test() ->
  Json = "{\"name\": \"bob\", \"friends\": [\"jane\", \"stan\"], \"info\": { \"age\": 24, \"gender\": \"male\" }}",
  Dict = web_request:json_to_dict(Json),
  <<"bob">> = dict:fetch(<<"name">>, Dict),
  [<<"jane">>, <<"stan">>] = dict:fetch(<<"friends">>, Dict),
  InfoDict = dict:fetch(<<"info">>, Dict),
  24 = dict:fetch(<<"age">>, InfoDict).

tuple_to_dict_test() ->
  Stuct = {{foo, bar}, {list, [one, two]}, {nested, {{baz, boo}}}},
  Dict = web_request:tuple_to_dict(Stuct),
  bar = dict:fetch(foo, Dict),
  [one, two] = dict:fetch(list, Dict),

  NestedDict = dict:fetch(nested, Dict),
  boo = dict:fetch(baz, NestedDict).
