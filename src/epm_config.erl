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

parse(Path, Pkg) ->
	lists:foldl(fun(Plugin, Acc) ->
		epm_utils:debug("parse ~s: ~s", [Path, Plugin]),
		case Plugin:parse(Path, Acc) of
			#cfg{} = Cfg ->
				merge(Pkg, Cfg, Acc);
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
		"~s",
		[render_repos(Repos), render_deps(Cfg) ]).

render_repos(Repos) ->
	lists:map(fun({Alias, _Backend, URL, Pkgs}) ->
		io_lib:format("= ~s (~s); ~B packages~n", [Alias, URL, length(Pkgs)])
	end, Repos).

render_deps(Cfg) ->
	lists:map(fun([<<"root">>|Path]) ->
		Pkg = (dep_lens(Path))(Cfg),
		Repo = [X || {X,_,_} <- Pkg#dep.repo],
		io_lib:format("= ~s @ ~p~n", [epm_utils:binjoin(Path, <<$.>>), Repo])
	end, Cfg#cfg.paths).


dep_lens(Path) ->
	lists:foldr(fun comp_lens/2, fun comp_cfg_lens/1, Path).

comp_cfg_lens(#cfg{deps = D}) -> D;
comp_cfg_lens(#dep{deps = D}) -> D.

comp_lens(Name, PA) ->
	fun(R) -> lists:keyfind(Name, #dep.name, PA(R)) end.
