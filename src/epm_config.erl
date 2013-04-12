-module(epm_config).

-export([
	  parse/1
	, parse/3
	, print/1
	, deps/1
	, repos/1
	]).

-include("epm.hrl").

parse(Path) ->
	parse(Path, #cfg{}, #dep{name = <<"root">>}).

parse(Path, Cfg, Pkg) ->
	CfgTokens = string:tokens(os:cmd(?epmpp(Path)), "\n"),
	{_, Cfg2} = lists:foldl(fun
		(<<"repositories<<">>, {_, Acc}) ->
			{repo, Acc};
		(<<"repositories<-">>, {_, Acc}) ->
			{repo, Acc};
		(<<"dependencies<<">>, {_, Acc}) ->
			{dep, Acc};
		(<<"dependencies<-">>, {_, Acc}) ->
			{dep, Acc};
		(<<" ", Arg/binary>>, {repo, #cfg{repos = R} = Acc}) ->
			case parse_repo(Arg) of
				{_,_,_,_} = Repo ->
					epm_utils:debug("add repo: ~s", [Arg]),
					{repo, Acc#cfg{
						repos = [Repo | R]}};
				false ->
					{repo, Acc}
			end;
		(<<" ", Arg/binary>>, {dep, #cfg{deps = D} = Acc}) ->
			epm_utils:debug("add dep: ~s", [Arg]),
			Dep = #dep{} = parse_dep(Arg),
			{dep, add_dep(Acc, Dep, Pkg)}
	end, {none, Cfg}, [list_to_binary(X) || X <- CfgTokens]),
	Deps = lists:map(fun(#dep{} = Dep) ->
		epm_deps:match_repos(Dep, Cfg)
	end, Cfg2#cfg.deps),
	Cfg2#cfg{deps = Deps}.

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

parse_dep(Arg) ->
	[Name|Opts] = binary:split(Arg, [<<$:>>, <<$@>>, <<$=>>], [global]),
	{_, Ret} = lists:foldl(fun(Opt, {Pos, Dep}) ->
			NewPos = Pos + size(Opt) + 1,
			case binary:part(Arg, {Pos, 1}) of
				<<$:>> ->
					{NewPos, Dep#dep{ref = Opt}};
				<<$@>> ->
					{NewPos, Dep#dep{repos = [Opt|Dep#dep.repos]}};
				<<$=>> ->
					{NewPos, Dep#dep{version = Opt}}
			end
		end, {size(Name), #dep{name = Name}}, Opts),
	Ret.

add_dep(#cfg{deps = Deps, paths = Paths} = Cfg, Dep, #dep{name = <<"root">>}) ->
	Cfg#cfg{
		  paths = [[<<"_">>, Dep#dep.name]|Paths]
		, deps = [Dep | Deps]};
add_dep(#cfg{deps = Deps} = Cfg, Dep, #dep{name = Name, deps = SubDeps} = Pkg) ->
	case lists:keyfind(#dep.name, Name, Deps) of
		Pkg ->
			Pkg2 = Pkg#dep{deps = [Dep|SubDeps]},
			Cfg#cfg{deps = lists:keyreplace(Name, #dep.name, Deps, Pkg2)};
		false ->
			exit(dep_discrepancy, [Dep, Pkg])
	end.

parse_repo(Repo) ->
	{Alias, URL} = case binary:split(Repo, <<"<-">>) of
		[A, U] -> {A, U};
		[U] -> {epm_utils:get_domain(U), U} end,
	case match_backend(URL) of
		{ok, Backend} ->
			{Alias2, Repos} = Backend:fetch(Alias, URL),
			{Alias2, Backend, URL, Repos};
		false ->
			epm_utils:info("no matching backends for '~s'", [Alias]),
			false
	end.

match_backend(URL) ->
	Backends = [{Backend, Backend:match(URL)} || Backend <- ?backends],
	case [{X,Y} || {X,Y} <- Backends, false =/= Y] of
		[{Backend, true}|_] ->
			{ok, Backend};
		[] ->
			false
	end.

deps(Cfg) ->
	Cfg#cfg.deps.

repos(Cfg) ->
	Cfg#cfg.repos.
