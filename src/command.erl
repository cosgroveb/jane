-module(command).
-export([call/2]).
-include_lib("jane.hrl").
-include_lib("command.hrl").

%%%===================================================================
%%% API
%%%===================================================================

call(Sender, Body) ->
  case eval(commands(), Sender, Body) of
    error -> {error, no_command};
    Output -> {ok, Output}
  end.

%%%===================================================================
%%% Private
%%%===================================================================

call_action(Action, Sender, Body) when is_function(Action) ->
  Action(Sender, Body);
call_action(_, _, _) ->
  error.

call_function(Mod, Fun) ->
  fun(_,_) -> apply(Mod, Fun, []), "Ok" end.

eval([], _Sender, _Body) ->
  error;
eval([Command|Commands], Sender, Body) ->
  #command{matches=Matches, action=Action, subcommands=SubCommands} = Command,
  PaddedBody = string:join([" ", Body, " "], ""),
  PaddedMatches = string:join(["\s", Matches, "\s"], ""),

  case re:run(PaddedBody, PaddedMatches) of
    nomatch ->
      eval(Commands, Sender, Body);
    _ ->
      case eval(SubCommands, Sender, Body) of
        error -> call_action(Action, Sender, Body);
        Output -> Output
      end
  end.

help(Commands) ->
  help("", Commands).

help(_Prefix, []) ->
  "";
help(Prefix, [#command{matches=Matches, description=Description, subcommands=SubCommands}|Commands]) ->
  SubCommandOutput = help("    \\_ ", SubCommands),
  string:concat(string:join([Prefix, Matches, ": ", Description, "\n", SubCommandOutput], ""), help(Prefix, Commands)).

shell_exec(ShellCommand) ->
  fun(_, _) -> os:cmd(ShellCommand) end.

%%%===================================================================
%%% Commands
%%%===================================================================

commands() -> [

  #command {
    matches = "stop",
    description = "Stops jane",
    action = call_function(jane_xmpp_server, silence)
  },

  #command {
    matches = "start",
    description = "Starts jane",
    action = call_function(jane_xmpp_server, unsilence)
  },

  #command {
    matches = "(hello|hi|hey|whats up)",
    description = "Greets you",
    action = fun(Sender, _Body) ->
      string:concat("Hello ", Sender)
    end
  },

  #command {
    matches = "help",
    description = "This command",
    action = fun(_Sender, _Body) ->
      help(commands())
    end
  },

  #command {
    matches = "mingle",
    description = "Finds a mingle card and returns title",
    action = fun(_Sender, Body) ->
      Card = lists:last(string:tokens(Body, " ")),
      CardNum = string:sub_string(Card, 2),
      Url = mingle_service:get_url(CardNum),
      {Name, _Description} = mingle_service:fetch_card(CardNum),

      string:join([Name, " (", Url, ")"], "")
    end,
    subcommands = [
      #command {
        matches = "full",
        description = "Also prints the body of the card",
        action = fun(_Sender, Body) ->
          Card = lists:last(string:tokens(Body, " ")),
          CardNum = string:sub_string(Card, 2),
          Url = mingle_service:get_url(CardNum),
          {Name, Description} = mingle_service:fetch_card(CardNum),

          string:join([Name, " (", Url, ")", "\n", Description], "")
        end
      }
    ]
  },

  #command {
    matches = "(whats on|what is on)",
    description = "Tells you what is on something",
    subcommands = [
      #command {
        matches = "qa",
        action = shell_exec("curl -s -k \"https://qa.braintreegateway.com/revision\" | awk '{ print $1 }'")
      },

      #command {
        matches = "qa2",
        action = shell_exec("curl -s -k \"https://qa2.braintreegateway.com/revision\" | awk '{ print $1 }'")
      },

      #command {
        matches = "(sandbox|sand)",
        action = shell_exec("curl -s -k \"https://sandbox.braintreegateway.com/revision\" | awk '{ print $1 }'")
      },

      #command {
        matches = "(production|prod)",
        action = shell_exec("curl -s -k \"https://www.braintreegateway.com/revision\" | awk '{ print $1 }'")
      }
    ]
  },

  #command {
    matches = "jukebox",
    action = fun(_Sender, _Body) ->
      Song = web_request:get_json("http://jukebox2.local/playlist/current-track"),
      Title = dict:fetch(<<"title">>, Song),
      Artist = dict:fetch(<<"artist">>, Song),
      string:join([binary_to_list(Title), " by ", binary_to_list(Artist)], "")
    end,
    subcommands = [
      #command {
        matches = "(whats playing|what is playing|playing|current)",
        action = fun(_Sender, _Body) ->
          Song = web_request:get_json("http://jukebox2.local/playlist/current-track"),
          Title = dict:fetch(<<"title">>, Song),
          Artist = dict:fetch(<<"artist">>, Song),
          string:join([binary_to_list(Title), " by ", binary_to_list(Artist)], "")
        end
      },

      #command {
        matches = "(whos song is this|song owner)",
        action = fun(_Sender, _Body) ->
          Song = web_request:get_json("http://jukebox2.local/playlist/current-track"),
          dict:fetch(<<"owner">>, Song)
        end
      },

      #command {
        matches = "(play|start)",
        action = fun(_Sender, _Body) ->
          web_request:get("http://jukebox2.local/player/play"),
          "I started jukebox"
        end
      },

      #command {
        matches = "pause",
        action = fun(_Sender, _Body) ->
          web_request:get("http://jukebox2.local/player/pause"),
          "I paused jukebox"
        end
      }
    ]
  },

  #command {
    matches = "(tweet|twitter)",
    action = fun(_Sender, Body) ->
      Url = lists:last(string:tokens(Body, " ")),
      TweetID = lists:last(string:tokens(Url, "/")),
      TweetUrl = string:concat("https://api.twitter.com/1/statuses/show.json?id=", TweetID),

      Tweet = web_request:get_json(TweetUrl),
      Text = binary_to_list(dict:fetch(<<"text">>, Tweet)),
      User = binary_to_list(dict:fetch(<<"screen_name">>, dict:fetch(<<"user">>, Tweet))),

      string:join(["\"", Text, "\" - @", User], "")
    end
  },

  #command {
    matches = "is it raining in chicago",
    action = fun(_Sender, Body) ->
      City = lists:last(string:tokens(Body, " ")),
      ApiUrl = string:concat("http://isitraining.in/", City),
      {ok, _StatusCode, _Headers, ResBody} = web_request:get(ApiUrl),

      [_|[WeatherConditions|_]] = re:split(ResBody, "</?h2>"),
      DegreedConditions = re:replace(WeatherConditions, "&deg;", "°", [global, {return,list}]),
      re:replace(DegreedConditions, "</?.*?/?>", "", [global, {return,list}])
    end
  }

].
