-module(epm_config).

-export([
	  parse/1
	, deps/1
	, repos/1
	]).

-include("epm.hrl").

parse(Path) ->
	CfgTokens = string:tokens(os:cmd(?epmpp(Path)), "\n"),
	{_, Cfg} = lists:foldl(fun
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
			{dep, Acc#cfg{deps = [Dep|D]}}
	end, {none, #cfg{}}, [list_to_binary(X) || X <- CfgTokens]),
	Deps = lists:map(fun(#dep{} = Dep) ->
		epm_deps:match_repos(Dep, Cfg)
	end, Cfg#cfg.deps),
	Cfg#cfg{deps = Deps}.

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
