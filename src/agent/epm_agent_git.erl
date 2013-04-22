-module(epm_agent_git).

-export([
	  name/0
	, init/1
	, fetch/2
	, sync/2
	, status/2
	]).

name() ->
	<<"git">>.

init(Pkg0) ->
	AbsName = lists:reverse(epm_pkg:get(absname, Pkg0)),

	%% Set the state of codepath, this is called as last step in epm:parse()
	epm_pkg:set(cfghook
	, fun(Cfg) ->
		Pkg1 = epm:get({dep, AbsName}, Cfg),
		Pkg = case epm_pkg:get({agent, ref}, Pkg1) of
			undefined -> epm_pkg:set({agent, ref}, any, Pkg1);
			_ -> Pkg1 end,

		Synced = case status(Pkg, Cfg) of
			ok ->
				true;
			missing ->
				epm:log(info, "agent:git: ~s:~s=~s not checked out"
					, epm_pkg:get([catalog, name, version], Pkg)),
				false;
			stale ->
				epm:log(info, "agent:git: ~s:~s=~s requires updated"
					, epm_pkg:get([catalog, name, version], Pkg)),
				false;
			{error, _} ->
				error end,

		NewPkg = epm_pkg:set(synced, Synced, Pkg),
		epm:set({dep, AbsName}, NewPkg, Cfg)
	  end
	, Pkg0).

-spec fetch(epm_pkg:pkg(), epm:cfg()) -> ok | {error, Reason :: term()}.
fetch(Pkg, Cfg) ->
	epm:log(notice, "agent:git: fetching ~s=~s (~s) from ~p"
			, epm_pkg:get([pkgname, version, name, catalog], Pkg)),

	fetch_pkg(Pkg, epm_pkg:get(catalog, Pkg), Cfg).

-spec sync(epm_pkg:pkg(), epm:cfg()) -> ok | {error, Reason :: term()}.
sync(Pkg, Cfg) ->
	epm:log(notice, "agent:git: syncing ~s=~s (~s) from ~p"
			, epm_pkg:get([pkgname, version, name, catalog], Pkg)),

	fetch_pkg(Pkg, epm_pkg:get(catalog, Pkg), Cfg).

-spec status(epm_pkg:pkg(), epm:cfg()) -> ok | stale | missing | {error, Reason :: term()}.
status(Pkg, Cfg) ->
	epm:log(notice, "agent:git: syncing ~s=~s (~s) from ~p"
			, epm_pkg:get([pkgname, version, name, catalog], Pkg)),

	status(Pkg, epm_pkg:get(catalog, Pkg), Cfg).

-spec status(epm_pkg:pkg(), epm_catalog:catalog(), epm:cfg())
	-> ok | stale | missing | {error, Reason :: term()}.
status(Pkg, Catalog, Cfg) ->
	CodePath = buildpath(Pkg, false, Cfg),

	update_cache(Pkg, Catalog, Cfg, false),
	HasCode = filelib:is_dir(CodePath),
	if
		HasCode ->
			epm_git:status(CodePath, epm_pkg:get({agent, ref}, Pkg), Cfg);
		true ->
			missing
	end.


%% Fetch a package, will call `git fetch` on cache and code copy.
%% Non-existing paths will be cloned.
fetch_pkg(Pkg, Catalog, Cfg) ->
	update_cache(Pkg, Catalog, Cfg),

	CodePath = buildpath(Pkg, false, Cfg),

	Ref = epm_pkg:get({agent, ref}, Pkg),

	case filelib:is_dir(CodePath) of
		true ->
			ok = epm_git:fetch(CodePath, Cfg),
			ok = epm_git:checkout(CodePath, Ref, Cfg);
		false ->
			CachePath = buildpath(Pkg, true, Cfg),
			ok = epm_git:clone(CodePath, Ref, CachePath, Cfg)
	end.

update_cache(Pkg, Catalogs, Cfg) ->
	update_cache(Pkg, Catalogs, Cfg, true).

update_cache(Pkg, Catalogs, Cfg, AutoFetch) ->
	CachePath = buildpath(Pkg, true, Cfg),

	{ok, Remote} = case epm_pkg:get({agent, remote}, Pkg) of
			undefined when [] =/= Catalogs->
				{ok, Catalog} = get_catalog(Catalogs, Cfg),
				Mod = epm_catalog:get(module, Catalog),
				Mod:resource(Pkg, Catalog);
			Remote0 when Remote0 =/= undefined -> {ok, Remote0} end,

	case {filelib:is_dir(CachePath), AutoFetch and epm:env(autofetch)} of
		{true, true} ->
			ok = epm_git:fetch(CachePath, Cfg);
		{false, true} ->
			epm:log(info, "clone:  ~p -> ~p", [Remote, CachePath]),
			ok = epm_git:clone(CachePath, "master", Remote, Cfg);
		{_, false} ->
			ok
	end.

get_catalog([], _) ->
	false;
get_catalog([CtName | Tail], Cfg) ->
	case epm_catalog:select({name, CtName}, Cfg) of
		[] ->
			get_catalog(Tail, Cfg);
		[Catalog | _] ->
			{ok, Catalog}
	end.

%% builds target path for pkg, if Cache =:= true then build the cached path
buildpath(Pkg, Cache, _Cfg) ->
	% case epm:get(append_versions, true, Cfg) of
	Suffix = case epm:env(append_versions, true) of
		true ->
			case epm_pkg:get(version, Pkg) of
				any -> <<>>;
				Version -> <<$-, Version/binary>> end;
		false ->
			<<>> end,

	case Cache of
		true ->
			CacheDir = case epm_pkg:get(catalog, Pkg) of
				[] -> <<"default">>;
				[M | _] -> M end,

			filename:join([epm:env(cachedir, <<".cache">>)
				, "dist"
				, epm_os:escape_filename(CacheDir)
				, epm_pkg:get(name, Pkg)
				 ]);
		false ->
			filename:join([epm:env(libdir, <<"lib">>)
				, <<((epm_pkg:get(name, Pkg)))/binary, Suffix/binary>>
				])
	end.
