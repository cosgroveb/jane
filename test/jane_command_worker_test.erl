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
          description = "says hello world",
          subcommands = [
            #command {
              matches = "foo",
              description = "bar"
            }
          ]
        }
      ]
    }
  ],

  Output = command:help(Commands),
  ExpectedOutput = "hello: says hello\n    \\_ world: says hello world\n           \\_ foo: bar\n\n",
  ?assertEqual(ExpectedOutput, Output).
