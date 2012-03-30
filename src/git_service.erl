-module(git_service).
-compile([export_all]).
-include_lib("jane.hrl").

% Public

range_log(ObjectA, ObjectB, RepoUrl) ->
  Command = string:join(["log --oneline ", "origin/", ObjectA, "...", "origin/", ObjectB], ""),
  repo_exec("fetch --all", RepoUrl),
  repo_exec(Command, RepoUrl).

show(Sha, RepoUrl) ->
  Command = string:join(["show -s", Sha], " "),
  repo_exec(Command, RepoUrl).

% Private

list_repos() ->
  DefaultRepos = ?app_env(git_repos),
  string:join(lists:map(fun({Name, _Url}) -> Name end, DefaultRepos), ", ").

get_repo(RepoName) ->
  DefaultRepos = dict:from_list(?app_env(git_repos)),
  case dict:is_key(RepoName, DefaultRepos) of
    true -> dict:fetch(RepoName, DefaultRepos);
    false -> error
  end.

extract_repo_name(Url) ->
  [Repo|_Rest] = string:tokens(lists:last(string:tokens(Url, ":")), "."),
  Repo.

is_repo(RepoPath) ->
  filelib:is_dir(repo_git_path(RepoPath)).

repo_git_path(RepoPath) ->
  string:join([RepoPath, ".git"], "/").

repo_exec(Command, RepoUrl) ->
  RepoPath = extract_repo_name(RepoUrl),
  case is_repo(RepoPath) of
    false -> clone(RepoUrl, RepoPath);
    _ -> ok
  end,

  io:fwrite("Exec: ~p~n", [Command]),
  os:putenv("GIT_DIR", repo_git_path(RepoPath)),
  exec(Command).

exec(Command) ->
  os:cmd(string:join(["git", Command], " ")).

clone(URL, TargetDir) ->
  filelib:ensure_dir(TargetDir),
  exec(string:join(["clone", URL, TargetDir], " ")),
  ok.
