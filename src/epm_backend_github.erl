-module(epm_backend_github).

-export([
	  name/0
	, match/1
	, fetch/2
	, clone/4
	, update/4
	, status/4
	, status/5
	]).

-import(epm_utils, [
	  debug/2
	, err/2
	]).

-import(epm_backend_git, [
	]).

-define(api(User)
	, "https://api.github.com/users/" ++ binary_to_list(User) ++ "/repos").

-define(cache(Repo), <<"repo/", Repo/binary>>).

name() ->
	<<"github">>.

match(<<"git@github.com:", _/binary>>) -> true;
match(<<"ssh://git@github.com:", _/binary>>) -> true;
match(<<"git://github.com/", _/binary>>) -> true;
match(<<"https://github.com:", _/binary>>) -> true;
match(_) -> false.

fetch(Alias, Resource) ->
	case [X || X <- binary:split(Resource, [<<$/>>, <<$:>>], [global]), <<>> =/= X] of
		[_, <<"github.com">>, User] ->
			Alias2 = fetch_alias(Alias, User),
			case epm_utils:load_cache(?cache(Alias2)) of
				{ok, Terms} ->
					{Alias2, Terms};
				false ->
					{Alias2, lookup_repos(Alias2, ?api(User))}
			end;
		X ->
			err("github-backend: unknown resource: ~p~n~s"
				, [X,fetch_alias(Alias, Resource)])
	end.

fetch_alias(undefined, User) ->
	<<"github.", User/binary>>;
fetch_alias(Alias, _) ->
	Alias.

clone(Path, Repo, URL, Ref) ->
	epm_backend_git:clone(Path, Repo, URL, Ref).

update(Path, Repo, URL, Ref) ->
	epm_backend_git:update(Path, Repo, URL, Ref).

status(Path, Repo, URL, Dep) ->
	epm_backend_git:status(Path, Repo, URL, Dep).

status(Path, Repo, URL, Dep, Check) ->
	epm_backend_git:status(Path, Repo, URL, Dep, Check).

lookup_repos(Alias, User) ->
	lookup_repos(Alias, User, []).

lookup_repos(Alias, URL, Acc) ->
	case req(URL) of
		{ok,{{_,200,"OK"}, Headers, Body}} ->
			{match, Capture} = re:run(Body, "\"git_url\":\"([^\"]+)\""
				, [global, {capture, all_but_first, binary}]),
			Ret = lists:ukeymerge(1, [{get_repo_name(X), X} || [X] <- Capture], Acc),
			case lists:keyfind("link", 1, Headers) of
				{_, Link} ->
					case get_next_link(Link) of
						false ->
							epm_utils:save_cache(?cache(Alias), Ret),
							Ret;
						Link2 ->
							lookup_repos(Alias, Link2, Ret)
					end;
				false ->
					epm_utils:save_cache(?cache(Alias), Ret),
					Ret
			end;
		{ok,{{_,_,Status},_,_}} ->
			err("~s: Some error with fetchin repos from gh~n", [Status]),
			false
	end.

get_repo_name(<<"git://github.com/", Tail/binary>>) ->
	[_, Repo] = binary:split(Tail, <<"/">>),
	binary:replace(Repo, <<".git">>, <<>>).

req(URL) ->
	maybe_start_inets(),
	debug("req: ~s~n", [URL]),
	httpc:request(URL).

maybe_start_inets() ->
	lists:keymember(inets, 1, application:which_applications()) orelse
		[application:start(App) || App <- [inets, crypto, public_key, ssl]].

get_next_link(Link) ->
	get_next_link2(string:tokens(Link, ";,<> ")).

get_next_link2([]) -> false;
get_next_link2([Link, "rel=\"next\""|_]) -> Link;
get_next_link2([_,_|T]) -> get_next_link2(T).
