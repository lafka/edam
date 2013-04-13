-module(epm_config).

-export([
	  parse/1
	, parse/2
	, deps/1
	, repos/1
	, print/1
	]).

-include("epm.hrl").

parse(Path) ->
	parse(Path, #dep{name = <<"root">>}).

%% Parse configuration(s) for a directory, this can be merged with
%% a parent configuration to build a configuration hierarchy.
%% We will do a naive approach of just building the tree of known
%% dependencies and letting the initial caller checkout any deps.
%% To make sure we have a consistent tree the top initial caller must
%% fetch the dependencies and then re-parse all the configs.
parse(Path, Pkg) ->
	lists:foldl(fun(Plugin, Acc) ->
		epm_utils:debug("parse ~s: ~s", [Path, Plugin]),
		case Plugin:parse(Path, Acc) of
			#cfg{} = Cfg ->
				Cfg2 = merge(Pkg, Cfg, Acc),
				Cfg2#cfg{deps = [epm_deps:proc(X) || X <- Cfg#cfg.deps]};
			false ->
				Acc
		end
	end, #cfg{}, ?plugins).

merge(#dep{name = Name, deps = PkgDeps} = Pkg, A, B) ->
	Paths = [ [Pkg#dep.name|X] || X <- A#cfg.paths],
	Pkg2 = Pkg#dep{deps = lists:umerge(A#cfg.deps, PkgDeps)},
	CfgDeps = case Name of
		<<"root">> -> A#cfg.deps;
		Name -> lists:keyreplace(Name, #dep.name, B#cfg.deps, Pkg2) end,
	#cfg{ repos = lists:ukeymerge(1, A#cfg.repos, B#cfg.repos)
	    , deps = CfgDeps
	    , paths = lists:umerge(Paths, B#cfg.paths)}.

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
		"~s~n"
		"~s",
		[render_repos(Repos), render_deps(Cfg), render_msgs(Cfg)]).

render_repos(Repos) ->
	lists:map(fun({Alias, _Backend, URL, Pkgs}) ->
		io_lib:format("= ~s (~s); ~B packages~n", [Alias, URL, length(Pkgs)])
	end, Repos).

render_deps(Cfg) ->
	lists:map(fun([<<"root">>|Path]) ->
		Pkg = (dep_lens(Path))(Cfg),
		Repo = [X || {X,_,_} <- Pkg#dep.repo],
		io_lib:format("= ~s:~s ~p~n"
			, [epm_utils:binjoin(Path, <<$.>>), Pkg#dep.version, Repo])
	end, Cfg#cfg.paths).

render_msgs(Cfg) ->
	Alerts = lists:foldl(fun([<<"root">>|Path], Acc) ->
		Pkg = (dep_lens(Path))(Cfg),
		case Pkg#dep.consistency of
			consistent -> Acc;
			_ -> [Pkg|Acc] end
	end, [], Cfg#cfg.paths),
	case Alerts of
		[] ->
			"";
		Alerts ->
			io_lib:format(
				"Alerts:~n"
				"~s",
				[[format_alert(X) || X <- Alerts]])
	end.

format_alert(#dep{consistency = incomplete, name = Y}) ->
	io_lib:format("= ~s: no suitable publishers found~n", [Y]);
format_alert(#dep{consistency = missing, name = Y} = Dep) ->
	#dep{repos = [{R,_,U}|_]} = Dep,
	io_lib:format("= ~s: requires remote fetch from ~s:~s~n", [Y,R,U]);
format_alert(#dep{consistency = unknown, name = Y, version = Vsn}) ->
	io_lib:format("= ~s: missing build target (vsn: ~s)~n", [Y, Vsn]);
format_alert(#dep{consistency = stale, name = Y}) ->
	io_lib:format("= ~s: dependency has remote updates~n", [Y]).


dep_lens(Path) ->
	lists:foldr(fun comp_lens/2, fun comp_cfg_lens/1, Path).

comp_cfg_lens(#cfg{deps = D}) -> D;
comp_cfg_lens(#dep{deps = D}) -> D.

comp_lens(Name, PA) ->
	fun(R) -> lists:keyfind(Name, #dep.name, PA(R)) end.
