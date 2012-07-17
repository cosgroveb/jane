-module(command).
-export([commands/0]).
-include_lib("jane.hrl").
-include_lib("command.hrl").

%%%===================================================================
%%% Private
%%%===================================================================

call_function(Mod, Fun) ->
  fun(_,_) -> apply(Mod, Fun, []), "Ok" end.

help(Commands) ->
  help("", Commands).

help(_Prefix, []) ->
  "";
help(Prefix, [#command{matches=Matches, description=Description, subcommands=SubCommands}|Commands]) ->
  ClearedPrefix = string:join([" " || _ <- Prefix], ""),
  SubCommandOutput = help(string:concat(ClearedPrefix, "    \\_ "), SubCommands),
  Output = if
    Prefix == "" ->
      string:join([Prefix, Matches, ": ", Description, "\n", SubCommandOutput, "\n"], "");
    true ->
      string:join([Prefix, Matches, ": ", Description, "\n", SubCommandOutput], "")
  end,

  string:concat(Output, help(Prefix, Commands)).

%%%===================================================================
%%% Commands
%%%===================================================================

commands() -> [

  #command {
    matches = "(twitter.com)",
    pad_match = false,
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
    matches = "jane",
    description = "Speak to jane",
    action = fun(_Sender, _Body) ->
      NotFoundResponses = [
        "Sorry, I don't know what you mean",
        "I have no idea what you're talking about",
        "Hmm? Maybe ask for help"
      ],
      random:seed(erlang:now()),
      lists:nth(random:uniform(length(NotFoundResponses)), NotFoundResponses)
    end,

    subcommands = [
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
        matches = "scores",
        action = fun(_Sender, _Body) ->
          User = ?app_env(elovation_user),
          Pass = ?app_env(elovation_password),
          Url = string:join(["http://", User, ":", Pass, "@", ?app_env(elovation_url)], ""),

          Stats = web_request:get_json(Url),
          Ratings = dict:fetch(<<"ratings">>, Stats),
          RatingStings = lists:map(fun(R) ->
            Player = binary_to_list(dict:fetch(<<"player">>, R)),
            Score = integer_to_list(dict:fetch(<<"value">>, R)),
            string:join([Player, " (", Score, ")"], "")
          end, Ratings),

          string:join(RatingStings, ", ")
        end
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
        matches = "(ci|jenkins)",
        description = "Info about Jenkins",
        action = fun(_Sender, _Body) ->
          case jenkins_service:failing_builds() of
            [] -> "All builds are passing! Yay!";
            BuildList ->
              FailingBuilds = string:join(BuildList, ", "),
              string:concat("Failing builds: ", FailingBuilds)
          end
        end,

        subcommands = [
          #command {
            matches = "(failing|failing builds)",
            description = "List failing builds",
            action = fun(_Sender, _Body) ->
              case jenkins_service:failing_builds() of
                [] -> "All builds are passing! Yay!";
                BuildList ->
                  FailingBuilds = string:join(BuildList, ", "),
                  string:concat("Failing builds: ", FailingBuilds)
              end
            end
          },

          #command {
            matches = "(build|job)",
            description = "Get info about specific build",
            action = fun(_Sender, Body) ->
              Build = lists:last(string:tokens(Body, " ")),
              jenkins_service:build_info(Build)
            end
          },

          #command {
            matches = "(box|status|busy)",
            description = "Info about the build box",
            action = fun(_Sender, _Body) ->
              jenkins_service:box_status()
            end
          }
        ]
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
        action = fun(_Sender, Body) ->
          PaddedBody = string:join([" ", Body, " "], ""),

          [{_Matches, Url}|_]= lists:filter(fun({Matches, _Url}) ->
            PaddedMatches = string:join(["\s", Matches, "\s"], ""),
            case re:run(PaddedBody, PaddedMatches) of
              nomatch -> false;
              _ -> true
            end
          end, ?app_env(whats_on_commands)),

          Cmd = string:join(["curl -s -k \"", Url, "\" | awk '{ print $1 }'"], ""),
          os:cmd(Cmd)
        end
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
          },

          #command {
            matches = "(search|find)",
            action = fun(_Sender, Body) ->
              WrapChar = case string:str(Body, "'") of
                0 -> "\"";
                _ -> "'"
              end,
              SearchString = lists:last(string:tokens(Body, WrapChar)),
              SearchRequest = string:join(string:tokens(SearchString, " "), "+"),

              Url = string:concat("http://jukebox2.local/library/search?q=", SearchRequest),
              Output = web_request:get_json(Url),
              Top5 = lists:map(fun(S) ->
                Title = binary_to_list(dict:fetch(<<"title">>, S)),
                Album = binary_to_list(dict:fetch(<<"album">>, S)),
                Artist = binary_to_list(dict:fetch(<<"artist">>, S)),
                string:join([Title, "  Album:", Album, "  Artist:", Artist], " ")
              end, lists:sublist(Output, 5)),

              case length(Top5) of
                0 -> "Jukebox didn't find anything";
                _ -> string:join(["Here are the top results:"|Top5], "\n")
              end
            end
          },

          #command {
            matches = "add",
            action = fun(_Sender, Body) ->
              WrapChar = case string:str(Body, "'") of
                0 -> "\"";
                _ -> "'"
              end,
              SearchString = lists:last(string:tokens(Body, WrapChar)),
              SearchRequest = string:join(string:tokens(SearchString, " "), "+"),

              Url = string:concat("http://jukebox2.local/library/search?q=", SearchRequest),
              Output = web_request:get_json(Url),
              Top5 = lists:map(fun(S) ->
                Title = binary_to_list(dict:fetch(<<"title">>, S)),
                Path= binary_to_list(dict:fetch(<<"path">>, S)),
                string:join([Title, " http://jukebox2.local/playlist/add/", Path], "")
              end, lists:sublist(Output, 5)),

              case length(Top5) of
                0 -> "Click the link of the song you want to add:";
                _ -> string:join(["Here are the top results:"|Top5], "\n")
              end
            end
          }
        ]
      },

      #command {
        matches = "git",
        subcommands = [
          #command {
            matches = "list",
            description = "list available repos",
            action = fun(_Sender, _Body) ->
              git_service:list_repos()
            end
          },

          #command {
            matches = "(last|latest)",
            description = "Shows the last commit",
            action = fun(_Sender, Body) ->
              RepoName = lists:last(string:tokens(Body, " ")),
              case git_service:get_repo(RepoName) of
                error ->
                  "I don't know about that git repo or object";
                RepoUrl ->
                  git_service:show("", RepoUrl)
              end
            end
          },

          #command {
            matches = "show",
            description = "<repo name/url> <commit>",
            action = fun(_Sender, Body) ->
              [Sha, RepoName|_] = lists:reverse(string:tokens(Body, " ")),

              case git_service:get_repo(RepoName) of
                error ->
                  "I don't know about that git repo";
                RepoUrl ->
                  git_service:show(Sha, RepoUrl)
              end
            end
          }
        ]
      },

      #command {
        matches = "(is it raining in chicago|weather)",
        action = fun(_Sender, Body) ->
          City = lists:last(string:tokens(Body, " ")),
          ApiUrl = string:concat("http://isitraining.in/", City),
          {ok, _StatusCode, _Headers, ResBody} = web_request:get(ApiUrl),

          [_|[WeatherConditions|_]] = re:split(ResBody, "</?h2>"),
          DegreedConditions = re:replace(WeatherConditions, "&deg;", "°", [global, {return,list}]),
          re:replace(DegreedConditions, "</?.*?/?>", "", [global, {return,list}])
        end
      },

      #command {
        matches = "environments",
        description = "List environments",
        action = fun(_Sender, _Body) ->
            environmentalist_service:index()
        end
      },

      #command {
        matches = "reserve",
        description = "Reserve an environment",
        action = fun(Reservee, Body) ->
            EnvironmentName = lists:last(string:tokens(Body, " ")),
            case environmentalist_service:reserve(Reservee, EnvironmentName) of
              reserved ->
                string:join(["Ok, I've reserved", EnvironmentName,
                    "for you,", Reservee], " ");
              not_reserved ->
                string:join(["I'm sorry, I wasn't able to reserve", EnvironmentName,
                    ". Are you sure it exists? Check here:", ?app_env(environmentalist_url)], " ")
            end
        end
      },

      #command {
        matches = "release",
        description = "Release an environment",
        action = fun(_Sender, Body) ->
            EnvironmentName = lists:last(string:tokens(Body, " ")),
            case environmentalist_service:release(EnvironmentName) of
              released ->
                string:join(["Ok,", EnvironmentName, "has been released"], " ");
              not_released ->
                string:join(["I'm sorry, I wasn't able to release", EnvironmentName,
                    ". Are you sure it exists? Check here:", ?app_env(environmentalist_url)], " ")
            end
        end
      },

      #command {
        matches = "who's on",
        description = "Show an environment",
        action = fun(_Sender, Body) ->
            EnvironmentName = lists:last(string:tokens(Body, " ")),
            case environmentalist_service:show(EnvironmentName) of
              {ok, EnvironmentStatus} ->
                EnvironmentStatus;
              not_found ->
                string:join(["I'm sorry, I wasn't able to find ", EnvironmentName,
                ". Are you sure it exists? Check here:", ?app_env(environmentalist_url)], " ")
            end
        end
      }

    ]
  }

].
