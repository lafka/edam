-module(epm_dep).

-export([
	  match_repos/2
	, show/1
	, show/2
	, update/1
	, has_update/1
	, proc/1
	]).

-export([
	  codepath/1
	, codepath/2
	, cachepath/1
	, cachepath/2
	]).

-include("epm.hrl").

match_repos(#dep{} = Dep, #cfg{repos = Repos}) ->
	match_repos2(Dep, Repos);
match_repos(Name, #cfg{deps = Deps, repos = Repos}) ->
	case lists:keyfind(Name, #dep.name, Deps) of
		#dep{repos = []} when Repos =:= [] ->
			epm_utils:err("dependency '~s' not found in any repos", [Name]),
			[];
		#dep{repos = []} = Dep ->
			match_repos2(Dep, Repos);
		false ->
			epm_utils:err("no such dependency '~p'", [Name]),
			[]
	end.

match_repos2(#dep{name = Name} = Dep, Repos) ->
	lists:foldl(fun({RepoName,Backend,_URL,Repo}, Acc) ->
		case lists:keyfind(Name, 1, Repo) of
			{Name, URL} ->
				Acc#dep{repo = [{RepoName, Backend, URL}|Acc#dep.repo]};
			_ ->
				Acc
		end end, Dep, Repos).

show(#dep{} = Dep) ->
	show(Dep, [version, ref, repo, repos]).

show(#dep{} = Dep, []) ->
	io_lib:format("= ~s @~p~n", [Dep#dep.name, [X || {X,_,_} <- Dep#dep.repo]]);
show(#dep{} = Dep, Inc) ->
	io_lib:format("= ~s @~p~n", [Dep#dep.name, [X || {X,_,_} <- Dep#dep.repo]])
	++ "========================\n" ++
	[ io_lib:format("=> ~s: ~p~n", [Attr, val(Dep, Attr)]) ||  Attr <- Inc].

val(#dep{version = Val}, version) -> Val;
val(#dep{ref = Val}, ref) -> Val;
val(#dep{repo = Val}, repo) -> Val;
val(#dep{repos = Val}, repos) -> Val;
val(#dep{deps = Val}, deps) -> Val.

%% Works by checking out a "base" directory, this keep tracks of your
%% remote and should always be in sync. When that is in place a clone
%% of the local repository is made using a ref. This is then placed in
%% lib/<name>-<vsn>. The result: having support for multiple versions
%% of a library in addition to a "central" cache for your project.
update(#dep{repo = [], repos = Repos, name = Name}) ->
	epm_utils:err("dependency '~s' has no canidates in ~p", [Name, Repos]);
update(#dep{repo = [{Repo,_,URL}|_], name = Name} = Dep) ->
	epm_utils:info("picking '~s' from ~s @ ~s", [Name, Repo, URL]),
	update_repo(Dep, true),
	update_repo(Dep, false).

update_repo(#dep{name = Name, repo = [{Repo,_,_}|_]} = Dep, Master) ->
	Path = case Master of
		true ->
			epm_utils:cache_path(<<"dist/", Repo/binary, "/", Name/binary>>);
		false when Dep#dep.version =:= any ->
			filename:join(["lib", <<Name/binary>>]);
		false ->
			Vsn = Dep#dep.version,
			filename:join(["lib", <<Name/binary, $-, Vsn/binary>>])
	end,
	do_update_repo(Path, Dep).

do_update_repo(Path, #dep{repo = [{Repo, Backend, URL}|_]} = Dep) ->
	case filelib:is_dir(Path) of
		true ->
			epm_utils:info("using local copy of ~s@~s from ~s~n"
				, [Dep#dep.name, Repo, Path]),
			Backend:update(Path, Repo, URL, Dep#dep.ref);
		false ->
			epm_utils:debug("creating local copy of ~s @ ~s from ~s"
				, [Dep#dep.name, Repo, URL]),
			Backend:clone(Path, Repo, URL, Dep#dep.ref)
	end.

-spec has_update(#dep{}) -> {Ret, Ret} when Ret :: true | false | missing | unknown.
has_update(#dep{repo = [{Repo, Backend, URL}|_], name = Name, ref = Ref} = Dep) ->
	Cachepath = cachepath(Dep),
	Codepath  = codepath(Dep),
	case {filelib:is_dir(Cachepath), filelib:is_dir(Codepath)} of
		{true, true} ->
			CacheUpdate = check_path_status(Cachepath, Backend, Repo, URL, Ref),
			LocalUpdate = check_path_status(Codepath, Backend, Repo, URL, Ref),
			case {CacheUpdate, LocalUpdate} of
				{ok, ok} -> false;
				{stale, _} -> true;
				{_, stale} -> true;
				{unknown,_} -> missing;
				{_,unknown} -> unknown
			end;
		{true, false} ->
			check_path_status(Cachepath, Backend, Repo, URL, Ref);
		{false, false} ->
			missing
	end.

check_path_status(Codepath, Backend, Repo, URL, Ref) ->
	Backend:status(Codepath, Repo, URL, Ref, false).

cachepath(#dep{} = Dep) ->
	cachepath(Dep, false).

cachepath(#dep{repo = [{Repo,_,_}|_], name = Name}, false) ->
	epm_utils:cache_path(<<"dist/", Repo/binary, "/", Name/binary>>);
cachepath(#dep{repo = [{Repo,_,_}|_], name = Name}, true) ->
	path_exists(epm_utils:cache_path(<<"dist/", Repo/binary, "/", Name/binary>>)).

codepath(#dep{} = Dep) ->
	codepath(Dep, false).

codepath(#dep{version = any, name = Name}, true) ->
	path_exists(epm_utils:lib_path(<<Name/binary>>));
codepath(#dep{version = Vsn, name = Name}, true) ->
	path_exists(epm_utils:lib_path(<<Name/binary, $-, Vsn/binary>>));
codepath(#dep{version = any, name = Name}, false) ->
	epm_utils:lib_path(<<Name/binary>>);
codepath(#dep{version = Vsn, name = Name}, false) ->
	epm_utils:lib_path(<<Name/binary, $-, Vsn/binary>>).

path_exists(Path) ->
	case filelib:is_dir(Path) of
		true ->
			{ok, Path};
		false ->
			{error, {missing, Path}}
	end.

proc(#dep{repo = []} = Dep) ->
	Dep#dep{consistency = incomplete};
proc(#dep{repo = [{Repo,Backend,_}|_]} = Dep) ->
	case cachepath(Dep, true) of
		{ok, Path} ->
			case proc_consistency(Path, Dep) of
				incomplete ->
					Dep#dep{consistency = incomplete};
				unknown ->
					Dep#dep{consistency = unknown};
				State ->
					Dep#dep{consistency = State}
			end;
		{error, {missing, Path}} ->
			epm_utils:debug("missing dep: ~s:~s -> ~s"
				, [Repo, Dep#dep.name, Path]),
			Dep#dep{consistency = missing}
	end.

proc_consistency(Path, #dep{repo = [{Repo,Backend,_}|_]} = Dep) ->
	Codepath = codepath(Dep),
	case proc_consistency2(Codepath, Dep, unknown) of
		#dep{consistency = unknown} ->
			epm_utils:info("checking out local copy of ~s:~s=~s"
				, [Repo, Dep#dep.name, Dep#dep.version]),
			Path2 = list_to_binary(Path),
			%% @todo 2013-04-13; lafka - Make fetch recursive flag
			case epm:get(autofetch) and not epm:get(dryrun) of
				true ->
					{ok,_} = Backend:clone(Codepath, Repo, Path2, Dep#dep.ref);
				false ->
					epm_utils:info("cannot determine dependencies for ~s"
						", requires fetch of ~s", [Codepath, Repo])
			end,
			ok;
		Dep2 ->
			epm_utils:info("found local copy of ~s:~s=~s"
				, [Repo, Dep#dep.name, Dep#dep.version]),
			Dep2#dep.consistency
	end.

proc_consistency2(Path, #dep{repo = [{Repo,Backend,URL}|_]} = Dep, Fb) ->
	case Backend:status(Path, Repo, URL, Dep#dep.ref, false) of
		ok -> Dep#dep{consistency = consistent};
		stale -> Dep#dep{consistency = stale};
		unknown -> Dep#dep{consistency = unknown};
		error -> Dep#dep{consistency = Fb}
	end.
