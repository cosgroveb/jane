-module(mingle_service).
-include("jane.hrl").
-export([fetch_card/5, fetch_card/1, get_url/3, get_url/1]).

fetch_card(Card) ->
  fetch_card(
    ?app_env(mingle_url), ?app_env(mingle_project), Card,
    ?app_env(mingle_user), ?app_env(mingle_password)
  ).

fetch_card(Domain, Project, Card, User, Pass) ->
  {ok, _StatusCode, _Headers, Body} = ibrowse:send_req(get_api_url(Domain, Project, Card), [], get, [], [
    {basic_auth, {User, Pass}}
  ]),
  Xml = erlsom:simple_form(Body),
  parse_xml_output(Xml).

get_api_url(Domain, Project, Card) ->
  XmlCard = string:concat(Card, ".xml"),
  string:join(["https:/", Domain, "api", "v2", "projects", Project, "cards", XmlCard], "/").

get_url(Card) ->
  get_url(?app_env(mingle_url), ?app_env(mingle_project), Card).

get_url(Domain, Project, Card) ->
  string:join(["https:/", Domain, "projects", Project, "cards", Card], "/").

parse_xml_output(Xml) ->
  {ok, {"card", [], Children}, "\n"} = Xml,
  [{"name", [], [Name]}, {"description", [], [Description]}|_Children] = Children,
  {Name, Description}.
