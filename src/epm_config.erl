-module(epm_config).

-export([
	  parse/1
	, parse/2
	, deps/1
	, repos/1
	, print/1
	]).

-export([dep_lens/1]).

-include("epm.hrl").

-define(root, <<"_root">>).

parse(Path) ->
	Res = parse(Path, #dep{name = ?root}),
	Res#cfg{paths = [[?root | X] || X <- Res#cfg.paths]}.

%% Parse configuration(s) for a directory, this can be merged with
%% a parent configuration to build a configuration hierarchy.
%% We will do a naive approach of just building the tree of known
%% dependencies and letting the initial caller checkout any deps.
%% To make sure we have a consistent tree the top initial caller must
%% fetch the dependencies and then re-parse all the configs.
parse(Path, Pkg) ->
	Res = lists:foldl(fun(Plugin, Acc) ->
		epm_utils:debug("parse ~s: ~s", [Path, Plugin]),
		case Plugin:parse(Path, Acc) of
			#cfg{} = Cfg ->
				Cfg2 = Cfg#cfg{
					deps = [epm_dep:proc(X) || X <- Cfg#cfg.deps]},
				merge(Cfg2, Acc);
			false ->
				Acc
		end
	end, #cfg{}, ?plugins),
	lists:foldl(fun(#dep{} = Dep, Acc) ->
		DepPath = epm_dep:codepath(Dep),
		DepCfg = parse(DepPath, Dep),
		pmerge(Dep, DepCfg, Acc)
	end, Res, Res#cfg.deps).

%% pmerge: merge a child configuration
pmerge(#dep{name = Name} = Pkg, A, B) ->
	Paths = [ [Name | X] || X <- A#cfg.paths],
	Deps = lists:keyreplace(Name
		, #dep.name
		, B#cfg.deps
		, Pkg#dep{deps = A#cfg.deps}),
	B#cfg{repos = lists:ukeymerge(1, A#cfg.repos, B#cfg.repos)
		, paths = lists:umerge(Paths, B#cfg.paths)
		, deps = Deps}.

%% merge:  merge configurations from the same directory (multiple plugins)
%% @todo lafka 2013-04-13; fix merge when multiple plugins share some dependencies
merge(A, B) ->
	B#cfg{repos = lists:ukeymerge(1, A#cfg.repos, B#cfg.repos)
		, deps  = lists:umerge(B#cfg.deps, A#cfg.deps)
	    , paths = lists:umerge(A#cfg.paths, B#cfg.paths)}.

deps(Cfg) ->
	Cfg#cfg.deps.

repos(Cfg) ->
	Cfg#cfg.repos.

print(#cfg{repos = Repos} = Cfg) ->
	io:format(
		"Config:~n"
		"=======~n~n"
		"Repositories:~n"
		"~s~n"
		"Dependencies:~n"
		"~s~n",
		[render_repos(Repos), render_deps(Cfg)]).

render_repos(Repos) ->
	lists:map(fun({Alias, _Backend, URL, Pkgs}) ->
		io_lib:format("= ~s (~s); ~B packages~n", [Alias, URL, length(Pkgs)])
	end, Repos).

render_deps(Cfg) ->
	lists:map(fun([?root | Path]) ->
		Pkg = (dep_lens(Path))(Cfg),
		[_|Head] = lists:reverse(Path),
		Parents = case epm_utils:binjoin(Head, <<" -> ">>) of
			<<>> -> <<>>;
			X -> io_lib:format("(~s)", [X]) end,
		io_lib:format("= ~s:~s ~s ~s; ~s~n"
			, [lists:last(Path)
				, Pkg#dep.version
				, Parents
				, format_repos(Pkg)
				, format_alert(Pkg)])
	end, Cfg#cfg.paths).

format_repos(#dep{repo = []}) ->
	<<>>;
format_repos(#dep{repo = Repos} = Dep) ->
	Repo = [X || {X,_,_} <- Repos],
	io_lib:format("~p", [Repo]).

format_alert(#dep{consistency = incomplete}) ->
	io_lib:format("no suitable publishers found", []);
format_alert(#dep{consistency = missing} = Dep) ->
	#dep{repo = [{R,_,U}|_]} = Dep,
	io_lib:format("requires remote fetch: remote -> ~s", [U]);
format_alert(#dep{consistency = unknown, version = Vsn}) ->
	io_lib:format("missing build target (vsn: ~s)", [Vsn]);
format_alert(#dep{consistency = stale}) ->
	io_lib:format("dependency has remote updates", []);
format_alert(_) ->
	"".


dep_lens(Path) ->
	lists:foldr(fun comp_lens/2
		, fun(X) -> X end
		, [ access_dep(X) || X <- Path]).

%access_deps(#dep{deps = D}) -> D.
access_dep(Name) -> 
	fun(#dep{deps = D}) -> lists:keyfind(Name, #dep.name, D);
	   (#cfg{deps = D}) -> lists:keyfind(Name, #dep.name, D) end.

comp_lens(Comp, Acc) ->
	fun(R) -> Acc(Comp(R)) end.
