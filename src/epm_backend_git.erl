-module(epm_backend_git).

-export([
	  name/0
	, match/1
	, fetch/2
	, clone/4
	, update/4
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
	{ok, CWD} = file:get_cwd(),
	epm_utils:debug("set cwd: ~s -> ~s", [CWD, Path]),
	maybe_update_remote(Path, URL),
	{ok, _} = epm_utils:cmd("git pull"),
	file:set_cwd(CWD);
update(Path, Repo, URL, Ref) ->
	{ok, CWD} = file:get_cwd(),
	epm_utils:debug("set cwd: ~s -> ~s", [CWD, Path]),
	maybe_update_remote(Path, URL),
	{ok, _} = epm_utils:cmd("git checkout ~s", [Ref]),
	file:set_cwd(CWD).

maybe_update_remote(Path, URL) ->
	ok = file:set_cwd(Path),
	URL2 = binary_to_list(URL) ++ "\n",
	case epm_utils:cmd("git remote -v | grep fetch | awk '{print $2}'") of
		{ok, URL2}->
			ok;
		{ok, OldURL} ->
			epm_utils:info("updating remote: ~p -> ~p", [OldURL, URL2]),
			{ok, _} = epm_utils:cmd("git remote set-url origin ~s ~s", [URL, OldURL]),
			{ok, _} = epm_utils:cmd("git remote update"),
			updated
	end.

