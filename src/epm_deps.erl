-module(epm_deps).

-export([
	  match_repos/2
	, show/1
	, show/2
	, update/1
	]).

-include("epm.hrl").

match_repos(#dep{} = Dep, #cfg{repos = Repos}) ->
	match_repos2(Dep, Repos);
match_repos(Name, #cfg{deps = Deps, repos = Repos}) ->
	case lists:keyfind(Name, #dep.name, Deps) of
		#dep{repos = []} when Repos =:= [] ->
			epm_utils:err("dependency '~s' not found in any repos", [Name]),
			false;
		#dep{repos = []} = Dep ->
			match_repos2(Dep, Repos);
		false ->
			epm_utils:err("no such dependency '~p'", [Name]),
			false
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
update(#dep{repo = [{Repo, Backend, URL}|_], name = Name, ref = Ref}) ->
	epm_utils:info("picking '~s' from ~s @ ~s", [Name, Repo, URL]),
	Path = epm_utils:cache_path(<<Repo/binary, "/", Name/binary>>),
	case filelib:is_dir(Path) of
		true ->
			epm_utils:info("using local copy of ~s@~s from ~s~n", [Name, Repo, Path]),
			Backend:update(Path, Repo, URL, Ref);
		false ->
			epm_utils:debug("creating local copy of ~s @ ~s from ~s", [Name, Repo, URL]),
			Backend:clone(Path, Repo, URL, Ref)
	end.
