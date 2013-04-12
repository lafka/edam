-module(epm_backend_git).

-export([
	  name/0
	, match/1
	, fetch/2
	, clone/4
	, update/4
	, status/4
	, status/5
	]).

name() ->
	<<"git">>.

match(_) ->
	false.

fetch(_, _) -> false.

clone(Path, Repo, URL, Ref) ->
	filelib:ensure_dir(Path),
	{ok, _} = epm_utils:cmd("git clone ~s ~s", [URL, Path]),
	update(Path, Repo, URL, Ref).

update(Path, Repo, URL, any) ->
	maybe_update_remote(Path, URL),
	{ok, _} = epm_utils:cmd("git pull origin master", [], Path);
update(Path, Repo, URL, Ref) ->
	maybe_update_remote(Path, URL),
	{ok, _} = epm_utils:cmd("git checkout ~s", [Ref], Path).

-spec status(any(), any(), any(), any()) -> ok | stale | unknown.
status(Path, _Repo, URL, Ref) ->
	status(Path, _Repo, URL, Ref, true).

-spec status(any(), any(), any(), any(), boolean()) -> ok | stale | unknown.
status(Path, _Repo, URL, Ref, CheckRem) ->
	Ret = case {maybe_update_remote(Path, URL), Ref} of
		{ok, any} -> check_git_log(Path, "master");
		{ok, Ref} -> check_git_log(Path, Ref);
		{updated, _} -> ok end,
	Ret.

check_git_log(Path, Ref) ->
	case epm_utils:cmd("git log HEAD..~s --oneline", [Ref], Path) of
		{ok, []} -> ok;
		{ok, _} -> stale;
		{error, _} -> error
	end.

maybe_update_remote(Path, URL) ->
	case epm_utils:cmd("git remote -v | grep fetch | awk '{print $2}'", [], Path) of
		{ok, <<URL, "\n">>} ->
			ok;
		{ok, OldURL} ->
			epm_utils:info("updating remote: ~p -> ~p", [OldURL, URL]),
			{ok, _} = epm_utils:cmd("git remote set-url origin ~s ~s", [URL, OldURL]),
			{ok, _} = epm_utils:cmd("git remote update"),
			updated
	end.

