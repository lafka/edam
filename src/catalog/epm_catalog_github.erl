-module(epm_catalog_github).

-export([
	  name/0
	, match/1
	, fetch/1
	, resource/2
	]).

resource(Pkg, Ctl) ->
	Name = epm_pkg:get(pkgname, Pkg),
	[Resource, User] = epm_catalog:get([resource, {opt, user}], Ctl),
	Parts = binary:split(Resource, [<<$/>>, <<$:>>], [global]),
	[Proto, <<"github.com">>, User | _] = [X || X <- Parts, <<>> =/= X],
	case Proto of
		<<"https">> ->
			{ok, <<"git://github.com/", User/binary, "/", Name/binary>>};
		Proto ->
			{ok, <<Proto/binary, "://github.com/", User/binary, "/", Name/binary>>}
	end.

name() ->
	<<"github">>.

match(<<"git@github.com:", _/binary>>) -> true;
match(<<"ssh://git@github.com:", _/binary>>) -> true;
match(<<"git://github.com/", _/binary>>) -> true;
match(<<"https://github.com/", _/binary>>) -> true;
match(_) -> false.

fetch(Ctl) ->
	Resource = epm_catalog:get(resource, Ctl),
	case [X || X <- binary:split(Resource, [<<$/>>, <<$:>>], [global]), <<>> =/= X] of
		[_, <<"github.com">>, User|_] ->
			Ctl2 = append_ctl(User, Ctl),
			Pkgs0 = load_cache(Ctl2, fun(NestedCtl) ->
				lookup_repos(NestedCtl, [])
			end),
			Pkgs = [create_pkg(N, R, Ctl2) || {N, R} <- Pkgs0],
			{ok, epm_catalog:set(pkgs, Pkgs, Ctl2)};
		Err ->
			Name = epm_catalog:get(name, Ctl),
			epm:log(error, "catalog: github: unknown resource: ~s:~s"
				, [Name, Resource]),
			epm:log(debug, "catalog: github: ~s: ~p", [Name, Err]),
			false
	end.

create_pkg(Name, Remote, Ctl) ->
	CtName = epm_catalog:get(name, Ctl),
	epm_pkg:new(Name
		, [template, {catalog, [CtName]}, {{agent, remote}, Remote}]).

append_ctl(User, Ctl0) ->
	Ctl = case epm_catalog:get(name, Ctl0) of
		undefined -> epm_catalog:set(name, <<"github.", User/binary>>, Ctl0);
		_ -> Ctl0 end,
	Opts = [ {opt, {user, User}}
		, {opt, {cache, epm_cache:path(["catalog", epm_catalog:get(name, Ctl)])}}
		, {resource, <<"https://github.com/", User/binary>>}],
	lists:foldl(fun({K, V}, Acc) -> epm_catalog:set(K, V, Acc) end
		, Ctl, Opts).

load_cache(Ctl, FallbackFun) ->
	case epm_cache:load(epm_catalog:get({opt, cache}, Ctl)) of
		{ok, Pkgs} ->
			Pkgs;
		{error, _} ->
			FallbackFun(Ctl)
	end.

rewrite(<<"https://github.com/", User/binary>>) ->
	"https://api.github.com/users/"  ++ binary_to_list(User) ++ "/repos".

lookup_repos(Ctl, Acc) ->
	lookup_repos(rewrite(epm_catalog:get(resource, Ctl)), Ctl, Acc).

lookup_repos(URL, Ctl, Acc) ->
	case req(URL) of
		{ok,{{_,200,"OK"}, Headers, Body}} ->
			{match, Capture} = re:run(Body, "\"git_url\":\"([^\"]+)\""
				, [global, {capture, all_but_first, binary}]),
			Ret = lists:ukeymerge(1, [{get_repo_name(X), X} || [X] <- Capture], Acc),
			case lists:keyfind("link", 1, Headers) of
				{_, Link} ->
					case get_next_link(Link) of
						false ->
							epm_cache:save(epm_catalog:get({opt, cache}, Ctl), Ret),
							Ret;
						Link2 ->
							lookup_repos(Link2, Ctl, Ret)
					end;
				false ->
					epm_cache:save(epm_catalog:get({opt, cache}, Ctl), Ret),
					Ret
			end;
		Err ->
			epm:log(error, "req: Some error with fetchin repos from gh: ~p~n", [Err]),
			[]
	end.

get_repo_name(<<"git://github.com/", Tail/binary>>) ->
	[_, Repo] = binary:split(Tail, <<"/">>),
	binary:replace(Repo, <<".git">>, <<>>).

req(URL) ->
	maybe_start_inets(),
	epm:log(debug, "req: ~s", [URL]),
	Headers = [{"User-Agent", "EPM HTTPC - http://github.com/lafka/epm"}],
	httpc:request(get, {URL, Headers}, [], []).

maybe_start_inets() ->
	lists:keymember(inets, 1, application:which_applications()) orelse
		[application:start(App) || App <- [inets, crypto, public_key, ssl]].

get_next_link(Link) ->
	get_next_link2(string:tokens(Link, ";,<> ")).

get_next_link2([]) -> false;
get_next_link2([Link, "rel=\"next\""|_]) -> Link;
get_next_link2([_,_|T]) -> get_next_link2(T).
