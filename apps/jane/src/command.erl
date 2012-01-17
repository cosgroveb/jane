-module(command).
-export([call/2]).
-include_lib("jane.hrl").

%%%===================================================================
%%% API
%%%===================================================================

call(Sender, Body) ->
  case find_and_run_command(Sender, Body, commands()) of
    error -> {error, no_command};
    Output -> {ok, Output}
  end.

%%%===================================================================
%%% Private
%%%===================================================================

find_and_run_command(_Sender, _Body, []) ->
  error;
find_and_run_command(Sender, Body, [{[], _CommandFun}|Commands]) ->
  find_and_run_command(Sender, Body, Commands);
find_and_run_command(Sender, Body, [{[CommandName|OtherCommandNames], CommandFun}|Commands]) ->
  case find_and_run_command(Sender, Body, [{CommandName, CommandFun}]) of
    error -> find_and_run_command(Sender, Body, [{OtherCommandNames, CommandFun}|Commands]);
    Output -> Output
  end;
find_and_run_command(Sender, Body, [{CommandName, CommandFun}|Commands]) ->
  WhiteSpaceCommand = string:join([" ", binary_to_list(CommandName), " "], ""),
  WhiteSpaceBody = string:join([" ", Body, " "], ""),
  case string:str(WhiteSpaceBody, WhiteSpaceCommand) of
    0 -> find_and_run_command(Sender, Body, Commands);
    _ -> CommandFun(Sender, Body)
  end.

%%%===================================================================
%%% Commands
%%%===================================================================

commands() ->
  [

    {[<<"hello">>, <<"hi">>], fun(Sender, _Body) ->
      string:concat("Hello ", Sender) end},

    {<<"uptime">>, fun(_Sender, _Body) ->
      os:cmd("uptime") end},

    {<<"where are your bins?">>, fun(_Sender, _Body) ->
      os:cmd("echo $PATH") end},

    {[<<"what is on sandbox?">>, <<"whats on sandbox">>, <<"sandbox revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://sandbox.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is on prod?">>, <<"whats on prod">>, <<"production revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://www.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is on qa?">>, <<"whats on qa">>, <<"qa revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://www.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is on qa2?">>, <<"whats on qa2">>, <<"qa2 revision">>], fun(_Sender, _Body) ->
      os:cmd("curl -s -k \"https://www.braintreegateway.com/revision\" | awk '{ print $1 }'") end},

    {[<<"what is playing">>, <<"whats playing">>, <<"what song is this">>], fun(_Sender, _Body) ->
      Song = web_request:get_json("http://jukebox2.local/playlist/current-track"),
      Title = dict:fetch(<<"title">>, Song),
      Artist = dict:fetch(<<"artist">>, Song),
      string:join([binary_to_list(Title), " by ", binary_to_list(Artist)], "")
    end},

    {<<"is it raining in">>, fun(_Sender, Body) ->
      City = lists:last(string:tokens(Body, " ")),
      ApiUrl = string:concat("http://isitraining.in/", City),
      {ok, _StatusCode, _Headers, ResBody} = web_request:get(ApiUrl),

      [_|[WeatherConditions|_]] = re:split(ResBody, "</?h2>"),
      DegreedConditions = re:replace(WeatherConditions, "&deg;", "°", [global, {return,list}]),
      re:replace(DegreedConditions, "</?.*?/?>", "", [global, {return,list}])
    end},

    {[<<"find card">>, <<"mingle">>, <<"card">>, <<"mingle card">>], fun(_Sender, Body) ->
      Card = lists:last(string:tokens(Body, " ")),
      CardNum = string:sub_string(Card, 2),
      Url = mingle_service:get_url(CardNum),
      {Name, _Description} = mingle_service:fetch_card(CardNum),

      string:join([Name, " (", Url, ")"], "")
    end}

  ].
