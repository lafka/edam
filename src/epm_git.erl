-module(epm_git).

%% API for working tree
-export([
	  checkout/2
	, fetch/1
	, clone/2
	, clone/3
	, status/2
	]).

%% API for working with Git info/config
-export([
	  remote/1
	, set_remote/2
	, ref/1
	, tags/1
	]).

-type ret() :: ok | {error, term()}.
-type ref() :: binary() | '_'.
-type remote() :: binary().

-spec checkout(filename:filename_all(), ref()) -> ret().
checkout(Path, '_') ->
	checkout(Path, hd(tags(Path)));
checkout(Path, Ref) ->
	Cmd = io_lib:format("git checkout ~s", [Ref]),

	%% @todo 2013-04-20; make dryrun part of config not env
	epm:unless(dryrun
		, fun() -> call(Cmd, Path, ok, {error, checkout}) end
		, fun() -> epm:log(command, "~s $ ~s", [Path, Cmd]) end).


-spec fetch(file:filename_all()) -> ret().
fetch(Path) ->
	Cmd = "git fetch --all",

	%% @todo 2013-04-20; make dryrun part of config not env
	epm:unless(dryrun
		, fun() -> call(Cmd, Path, ok, {error, fetch}) end
		, fun() -> epm:log(command, "~s $ ~s", [Path, Cmd]), ok end).


-spec clone(file:filename_all(), remote()) -> ret().
clone(Path, Remote) ->
	clone(Path, Remote, []).

-spec clone(file:filename_all(), remote(), [string()]) -> ret().
clone(Path, Remote, Opts0) ->
	Opts = string:join(Opts0, " "),
	Cmd1 = io_lib:format("git clone ~s ~s ~s", [Opts, Remote, Path]),

	%% @todo 2013-04-20; make dryrun part of config not env
	epm:unless(dryrun
		, fun() ->
			ok = call(Cmd1, ok, {error, clone})
		end
		, fun() ->
			{ok, Cwd} = file:get_cwd(),
			epm:log(command, "~s $ ~s", [Cwd, Cmd1])
		end).


-spec status(file:filename_all(), ref()) -> stale | ret().
status(Path, '_') ->
	case tags(Path) of
		[Tag | _] ->
			status(Path, Tag);
		[] ->
			status(Path, any)
	end;
status(Path, any) ->
	status(Path, ref(Path));
status(Path, Ref) ->
	case epm_os:cmd("git log HEAD..~s", [Ref], Path) of
		{ok, []} -> ok;
		{ok, _} -> stale;
		{error, Err} ->
			epm:log(error, "git: failed to get ~p @ ~p status:~n~p"
				, [Path, Ref, Err]),
			{error, status}
	end.

-spec remote(file:filename_all()) -> false | remote().
remote(Path) ->
	case epm_os:cmd("git remote -v | awk '/fetch/{print $2}'", [], Path) of
		{ok, []} -> false;
		{ok, Remote} -> Remote;
		{error, Err} ->
			epm:log(error, "git: failed to get status from '~s':~n~s"
				, [Path, Err]),
			{error, remote}
	end.

-spec set_remote(file:filename_all(), remote()) -> ret().
set_remote(Path, Remote) ->
	case remote(Path) of
		Remote ->
			ok;
		_ ->
			epm:unless(dryrun
				, fun() -> set_remote2(Path, Remote, true) end
				, fun() -> set_remote2(Path, Remote, false) end)
	end.

set_remote2(Path, Remote, true) ->
	case epm_os:cmd("git remote set-url origin ~s", [Remote], Path) of
		{ok, _} ->
			epm:log(info, "git: updating remote for ~s: ~s"
				, [Path, Remote]),
			ok;
		{error, Err} ->
			epm:log(error, "git: failed to update remote '~s':~n~s"
				, [Path, Err]),
			{error, set_remote}
	end;
set_remote2(Path, Remote, false) ->
	epm:log(command, "~s $ git remote set-url origin ~s", [Path, Remote]),
	ok.

-spec ref(file:filename_all()) -> false | ref().
ref(Path) ->
	Cmd = "git show --pretty=format:'%d' | awk '{print $2;exit}' | tr -d ',\)'",
	case epm_os:cmd(Cmd, [], Path) of
		{ok, Ref} -> Ref;
		{error, Err} ->
			epm:log(error, "git: failed to get ref from '~s':~n~s"
				, [Path, Err]),
			{error, ref}
	end.

-spec tags(file:filename_all()) -> [ref()].
tags(Path) ->
	case filelib:is_dir(Path) andalso epm_os:cmd("git tag -l", [], Path) of
		{ok, Tags} ->
			string:tokens(Tags, "\n");
		false ->
			[]
	end.

%% @private perform os call
call(Cmd, A, B) ->
	{ok, Cwd} = file:get_cwd(),
	call(Cmd, Cwd, A, B).

%% @private perform os call
call(Cmd, Path, A, B) ->
	case epm_os:cmd(Cmd, [], Path) of
		{ok, _} ->
			A;
		{error, Err} ->
			epm:log(error, "git: failed to execute: ~s~n~p", [Cmd, Err]),
			B
	end.
