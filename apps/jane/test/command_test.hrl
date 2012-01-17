find_and_run_command_test() ->
  Output = find_and_run_command("foo", "hello", commands()),
  ?assert(Output == "Hello foo").

commands_weather_test() ->
  ibrowse:start(),
  {ok, Output} = call("foo", "is it raining in dwchicago"),
  ?assert(Output == "No results! There might be something wrong with the city name.."),
  ibrowse:stop().

