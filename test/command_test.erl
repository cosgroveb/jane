-module(command_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("command.hrl").

call_action_test() ->
  ?assertEqual(error, command:call_action(x, x, x)),
  ?assertEqual(foo, command:call_action(fun(_,_) -> foo end, x, x)).

eval_no_match_test() ->
  Output = command:eval([], "Tester", "hello world"),
  ?assertEqual(error, Output).

eval_match_test() ->
  Commands = [

    #command {
      matches = "doesntmatch",
      action  = fun(_, _) ->
        "bad"
      end
    },

    #command {
      matches = "(hello|hi)",
      action  = fun(_, _) ->
        "foo"
      end
    }

  ],

  ?assertEqual("foo", command:eval(Commands, "Tester", "hello")),
  ?assertEqual("foo", command:eval(Commands, "Tester", "hi")).

eval_match_subcommand_test() ->
  Commands = [

    #command {
      matches = "doesntmatch",
      action  = fun(_, _) ->
        "bad"
      end
    },

    #command {
      matches = "hello",
      action  = fun(_, _) ->
        "foo"
      end,

      subcommands = [
        #command {
          matches = "world",
          action = fun(_, _) ->
            "bar"
          end
        }
      ]
    }

  ],

  Output = command:eval(Commands, "Tester", "hello world"),
  ?assertEqual("bar", Output).

eval_match_subcommand_nomatch_test() ->
  Commands = [

    #command {
      matches = "doesntmatch",
      action  = fun(_, _) ->
        "bad"
      end
    },

    #command {
      matches = "hello",
      action  = fun(_, _) ->
        "foo"
      end,

      subcommands = [
        #command {
          matches = "doesntmatch",
          action = fun(_, _) ->
            "bar"
          end
        }
      ]
    }

  ],

  Output = command:eval(Commands, "Tester", "hello world"),
  ?assertEqual("foo", Output).

eval_match_subcommand_nomatch_no_default_action_test() ->
  Commands = [
    #command {
      matches = "hello",

      subcommands = [
        #command {
          matches = "doesntmatch",
          action = fun(_, _) ->
            "bar"
          end
        }
      ]
    }

  ],

  Output = command:eval(Commands, "Tester", "hello world"),
  MatchOutput = command:eval(Commands, "Tester", "hello doesntmatch"),
  ?assertEqual(error, Output),
  ?assertEqual("bar", MatchOutput).

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
  ExpectedOutput = "hello: says hello\n    \\_ world: says hello world\n",
  ?assertEqual(ExpectedOutput, Output).

shell_exec_test() ->
  Fun = command:shell_exec("echo 'hi'"),
  ?assertEqual("hi\n", Fun(x, x)).

is_it_raining_in_test() ->
  meck:new(web_request),
  meck:expect(web_request, get, fun(_) -> {ok, x, x, "<h1>No</h1><h2>Conditions for <strong>Chicago, Illinois, United States</strong><br/>on Mon, 16 Jan 2012 7:10 pm CST: <strong>Cloudy</strong> (7&deg;C, 43&deg;F)</h2>"} end),

  {ok, Output} = command:call("foo", "is it raining in chicago"),
  meck:unload(web_request),

  ?assertEqual(Output, "Conditions for Chicago, Illinois, United Stateson Mon, 16 Jan 2012 7:10 pm CST: Cloudy (7°C, 43°F)").

