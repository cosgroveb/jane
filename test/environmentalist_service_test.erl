-module(environmentalist_service_test).
-include_lib("eunit/include/eunit.hrl").

environment_to_string_test() ->
  ReservedEnvironmentString = environmentalist_service:environment_to_string(
    <<"qa">>,
    <<"jane">>,
    <<"09/10/2012 00:00:00">>
  ),
  UnreservedEnvironmentString = environmentalist_service:environment_to_string(
    <<"qa">>,
    <<"">>,
    <<"09/10/2012 00:00:00">>
  ),
  ?assertEqual("qa was reserved by jane at 09/10/2012 00:00:00 \n", ReservedEnvironmentString),
  ?assertEqual("qa is not reserved.\n", UnreservedEnvironmentString).

