-module(command).
-export([call/2]).
-include_lib("jane.hrl").
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST). -include("../test/command_test.hrl"). -endif.

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
  case string:str(Body, binary_to_list(CommandName)) of
    0 -> find_and_run_command(Sender, Body, Commands);
    _ -> CommandFun(Sender, Body)
  end.

%%%===================================================================
%%% Commands
%%%===================================================================

commands() ->
  [

    {<<"is it raining in">>, fun(_Sender, Body) ->
      City = lists:last(string:tokens(Body, " ")),
      ApiUrl = string:concat("http://isitraining.in/", City),

      ApiResponse = ibrowse:send_req(ApiUrl, [], get, []),
      {ok, _StatusCode, _Headers, ResBody} = ApiResponse,
      [_|[WeatherConditions|_]] = re:split(ResBody, "</?h2>"),
      DegreedConditions = re:replace(WeatherConditions, "&deg;", "°", [global, {return,list}]),
      re:replace(DegreedConditions, "</?.*?/?>", "", [global, {return,list}])
    end},

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
      CurrentSong = jsonerl:decode(
        os:cmd("curl -s -H \"Accept: application/json\" http://jukebox2.local/playlist/current-track")),

      {_, _, _,
        {<<"artist">>, Artist}, {<<"owner">>, _Owner}, {<<"title">>, Title}, {<<"album">>, _Album},
        _, _, _, _} = CurrentSong,

      string:concat(binary_to_list(Title), string:concat(" by ", binary_to_list(Artist)))
    end},

    {[<<"find card">>, <<"mingle">>, <<"card">>, <<"mingle card">>], fun(_Sender, Body) ->
      [CardNum|_] = lists:reverse(string:tokens(Body, " ")),
      case length(CardNum) == 5 of
        false -> "Can't find card";
        true ->
          XmlUrl = string:join(["https://", ?app_env(mingle_url), "/api/v2/projects/", ?app_env(mingle_project), "/cards/", string:sub_string(CardNum, 2), ".xml"], ""),
          Url = string:join(["https://", ?app_env(mingle_url), "/projects/", ?app_env(mingle_project), "/cards/", string:sub_string(CardNum, 2)], ""),

          Response = ibrowse:send_req(XmlUrl, [], get, [], [
            {basic_auth, {?app_env(mingle_user), ?app_env(mingle_password)}}
          ]),

          {ok, _StatusCode, _Headers, ResBody} = Response,
          try erlsom:simple_form(ResBody) of
            Xml ->
              {ok, {"card", [], Children}, "\n"} = Xml,
              [{"name", [], [Name]}, {"description", [], [_Description]}|_Children] = Children,
              string:join([Name, " (", Url, ")"], "")
          catch
            _ -> "Sorry that seems to be an invalid card."
          end
      end
    end}

  ].
