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
	case binary:match(URL, [<<$@>>, <<"://">>]) of
		nomatch ->
			filelib:ensure_dir(Path),
			URL2 = filename:absname(URL),
			{ok, _} = epm_utils:cmd("git clone -l ~s ~s", [URL2, Path]),
			update(Path, Repo, URL2, Ref);
		_ ->
			{ok, _} = epm_utils:cmd("git clone ~s ~s", [URL, Path]),
			update(Path, Repo, URL, Ref)
	end.

update(Path, Repo, URL, any) ->
	maybe_update_remote(Path, URL),
	{ok, _} = epm_utils:cmd("git pull origin master", [], Path);
update(Path, _Repo, URL, Ref) ->
	maybe_update_remote(Path, URL),
	{ok, _} = epm_utils:cmd("git checkout ~s", [Ref], Path).

-spec status(any(), any(), any(), any()) -> ok | stale | unknown.
status(Path, _Repo, URL, Ref) ->
	status(Path, _Repo, URL, Ref, true).

-spec status(any(), any(), any(), any(), boolean()) -> ok | stale | unknown | error.
status(Path, Repo, URL, Ref, CheckRem) ->
	RemoteChanged = case has_updated_remote(Path, URL) of
		{true, _} -> true;
		false -> false end,
	case filelib:is_dir(Path) of
		true when CheckRem ->
			Ret = case {maybe_update_remote(Path, URL), Ref} of
				{ok, any} -> check_git_log(Path, "master");
				{ok, Ref} -> check_git_log(Path, Ref);
				{updated, _} -> ok end,
			Ret;
		_ when RemoteChanged ->
			epm_utils:info("remote: ~s needs remote change (~s)"
				, [Repo, URL]),
			check_git_log(Path, "master");
		true ->
			check_git_log(Path, "master");
		false ->
			missing
	end.

check_git_log(Path, Ref) ->
	case epm_utils:cmd("git log HEAD..~s --oneline", [Ref], Path) of
		{ok, []} -> ok;
		{ok, _} -> stale;
		{error, _} -> error
	end.

has_updated_remote(Path, URL) ->
	URL2 = binary_to_list(URL) ++ "\n",
	case epm_utils:cmd("git remote -v | grep fetch | awk '{print $2}'", [], Path) of
		{ok, URL2} -> false;
		{ok, Old} ->
			epm_utils:info("remote changed: ~s -> ~s", [Old, URL2]),
			{true, Old}
	end.

maybe_update_remote(Path, URL) ->
	case has_updated_remote(Path, URL) of
		false ->
			ok;
		{true, OldURL} ->
			epm_utils:info("updating remote: ~p -> ~p", [OldURL, URL]),
			{ok, _} = epm_utils:cmd("git remote set-url origin ~s", [URL], Path),
			{ok, _} = epm_utils:cmd("git remote update", [], Path),
			updated
	end.

