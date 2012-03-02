-module(command_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("command.hrl").

call_action_test() ->
  ?assertEqual(error, jane_command_worker:call_action(x, x, x)),
  ?assertEqual(foo, jane_command_worker:call_action(fun(_,_) -> foo end, x, x)).

eval_no_match_test() ->
  Output = jane_command_worker:eval_message([], "Tester", "hello world"),
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

  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hello")),
  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hi")).


eval_match_special_test() ->
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

  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hello?")),
  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hello??")),
  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hello!")),
  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hello,")),
  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hello.")),
  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hello;")),
  ?assertEqual("foo", jane_command_worker:eval_message(Commands, "Tester", "hi!")).

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

  Output = jane_command_worker:eval_message(Commands, "Tester", "hello world"),
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

  Output = jane_command_worker:eval_message(Commands, "Tester", "hello world"),
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

  Output = jane_command_worker:eval_message(Commands, "Tester", "hello world"),
  MatchOutput = jane_command_worker:eval_message(Commands, "Tester", "hello doesntmatch"),
  ?assertEqual(error, Output),
  ?assertEqual("bar", MatchOutput).
