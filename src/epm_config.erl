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
		case Plugin:parse(Path, Acc, Pkg) of
			#cfg{} = Cfg ->
				merge(Pkg, Cfg, Acc);
			false ->
				Acc
		end
	end, #cfg{}, ?plugins).

merge(_Pkg, A, _B) ->
	A.

deps(Cfg) ->
	Cfg#cfg.deps.

repos(Cfg) ->
	Cfg#cfg.repos.

print(#cfg{repos = Repos, paths = Paths}) ->
	io:format(
		"Config:~n"
		"=======~n~n"
		"Repositories:~n"
		"~s~n"
		"Dependencies:~n"
		"~s",
		[render_repos(Repos), render_paths(Paths) ]).

render_repos(Repos) ->
	lists:map(fun({Alias, _Backend, URL, Pkgs}) ->
		io_lib:format("= ~s (~s); ~B packages~n", [Alias, URL, length(Pkgs)])
	end, Repos).

render_paths(Paths) ->
	lists:map(fun([Repo|Path]) ->
		io_lib:format("= ~s @ ~s~n", [epm_utils:binjoin(Path, <<$.>>), Repo])
	end, Paths).
