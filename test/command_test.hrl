find_and_run_command_test() ->
  Output = find_and_run_command("foo", "hello", commands()),
  ?assert(Output == "Hello foo").

