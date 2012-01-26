-module(jane_command_worker_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("command.hrl").

help_test() ->
  Commands = [
    #command {
      matches = "hello",
      description = "says hello",
      subcommands = [
        #command {
          matches = "world",
          description = "says hello world"
        }
      ]
    }
  ],

  Output = command:help(Commands),
  ExpectedOutput = "hello: says hello\n    \\_ world: says hello world\n\n",
  ?assertEqual(ExpectedOutput, Output).

shell_exec_test() ->
  Fun = command:shell_exec("echo 'hi'"),
  ?assertEqual("hi\n", Fun(x, x)).
