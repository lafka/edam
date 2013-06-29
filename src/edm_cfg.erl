-module(edm_cfg).

-compile([{no_auto_import, [get/1]}]).

-define(ref, edm_cfg_server).

-record('edm_cfg.cfg', {
	  catalogs = [] :: [edm_cat:cat()]
	, deps :: digraph()
	, opts = [] :: [{atom(), term()}]
	, path :: binary()
	}).

-opaque cfg() :: #'edm_cfg.cfg'{}.

-export_type([cfg/0]).

-export([
	  new/1
	, parse/1
	, parse/2
	, resolve/1
	, save/1
	, configs/0
	, key/1
	, get/2
	, set/3
	, append_dep/3
	, iter/2
	]).

-spec new([{atom(), term()}]) -> cfg().
new(Opts) ->
	G = digraph:new(),
	_Root = digraph:add_vertex(G, edm_env:get('target.name', <<"_root">>)),
	new(Opts, #'edm_cfg.cfg'{deps = G}).

%% @private create a new configuration based on Cfg
new(Opts0, Cfg) ->
	%% Make sure erl libs catalog is available
	Catalogs = proplists:append_values(catalogs, Opts0),
	LocalCat = lists:filter(fun filter_sys_libs/1, Catalogs),
	IncludeSysLibs = edm_env:get(include_sys_libs, true),

	%% @todo olav 2013-06-24; Make option for not adding local erl libs
	Opts = if
		length(LocalCat) == 0 andalso IncludeSysLibs ->
			case edm_cat:new(erlang, list_to_binary(code:lib_dir())) of
				{ok, Cat0} ->
					Cat = edm_cat:set({pkgopt, state}, system, Cat0),
					edm_log:debug("appending local OTP libs to codepath (~s)", [code:lib_dir()]),
					[{catalogs, [Cat | Catalogs]}
						| proplists:delete(catalogs, Opts0)];
				false ->
					Opts0
			end;
		true -> Opts0
	end,

	lists:foldl(fun
		({K, V}, Acc) -> set(K, V, Acc);
		(A, Acc) when is_atom(A) ->  Acc
	end, Cfg, Opts).

filter_sys_libs(Cat) ->
	ErlLibs = code:lib_dir(),
	case edm_cat:get([module, resource], Cat) of
		[edm_catalog_local, ErlLibs] -> true;
		_ -> false
	end.

parse(Path) ->
	parse(Path, []).

parse(Path0, Opts) ->
	Path = filename:absname(Path0),
	Cfg = new(Opts, #'edm_cfg.cfg'{path = Path}),
	Pkg = edm_pkg:new(<<"_root">>),

	NewCfg = lists:foldl(fun(P, Acc) ->
		case P:parse(Acc, Pkg) of
			{ok, NewAcc} ->
				NewAcc;
			false ->
				Acc;
			{error, Err} ->
				exit({parser, Err})
		end
	end, Cfg, edm_env:get('parsers')),

	lists:member(save, Opts) andalso (ok = save(NewCfg)),

	configs().

configs() ->
	gen_server:call(?ref, {config, list}).

-spec save(cfg()) -> ok.
save(Cfg) ->
	edm_cfg_server:persist(Cfg).

key(catalogs) -> #'edm_cfg.cfg'.catalogs;
key(opts) -> #'edm_cfg.cfg'.opts;
key(path) -> #'edm_cfg.cfg'.path.

-spec get(atom(), cfg()) -> term().
get(Attrs, Cfg) when is_list(Attrs) ->
	[get(Attr, Cfg) || Attr <- Attrs];
get({opt, Key}, Cfg) ->
	case lists:keyfind(Key, 1, Cfg#'edm_cfg.cfg'.opts) of
		{Key, Val} ->
			Val;
		false ->
			undefined
	end;
get(Key, #'edm_cfg.cfg'{} = Pkg) when is_atom(Key) ->
	N = key(Key),
	erlang:element(N, Pkg).

-spec set(atom(), Val, cfg()) -> Val when Val :: term().
set({opt, K}, Val, #'edm_cfg.cfg'{opts = Opts} = Cfg) ->
	Cfg#'edm_cfg.cfg'{opts = lists:keystore(K, 1, Opts, {K, Val})};
set(Key, Val, #'edm_cfg.cfg'{} = Pkg) when is_atom(Key) ->
	N = key(Key),
	erlang:setelement(N, Pkg, Val).

-spec append_dep({Name, [Constraint], [Opt]}, digraph:vertex(), Cfg) -> Cfg
	when Name :: edm_pkg:name()
	   , Constraint :: edm_pkg:constraint()
	   , Opt :: {edm_pkg:opt(), term()}
	   , Cfg :: cfg().

append_dep({Dep, Constraint, Opts}, Parent, #'edm_cfg.cfg'{deps = G} = Cfg) ->
	Vtx = digraph:add_vertex(G, Dep),
	_E = digraph:add_edge(G, Parent, Vtx, {Constraint, Opts}),
	Cfg.

resolve(#'edm_cfg.cfg'{deps = G} = Cfg) ->
	{Resolved0, Unresolved} = lists:foldl(fun(E, {A, B}) ->
		case resolve2(G, E, Cfg) of
			{ok, Dep}    -> {[Dep | A], B};
			{error, Dep} -> {A, [Dep | B]}
		end
	end, {[], []}, digraph:edges(G)),

	Resolved = reduce_pkg_deps(Resolved0),

	{Resolved, Unresolved}.

resolve2(G, E, Cfg) ->
	{E, _ParentRef, Dep, {DepConstraints, DepOpts}} = digraph:edge(G, E),

	case edm_cat:resolve({Dep, DepConstraints, DepOpts}, Cfg) of
		[] -> {error, {Dep, DepConstraints}};
		Pkgs -> {ok, {Dep, Pkgs}}
	end.

%% Reduces list of dependencies, with possible duplicates, to the
%% minimum representation.
reduce_pkg_deps(Pkgs) ->
	[P || {_, [P|_]} <- reduce_pkg_deps(Pkgs, [])].

%% Pick first reference A for each pkg. Then for any additional reference
%% B, then update reference A with intersection of A and B.
reduce_pkg_deps([], Acc) ->
	Acc;

reduce_pkg_deps([{Name, Match} | Tail], Acc) ->
	VsnPos = edm_pkg:key(version),
	case lists:keyfind(Name, 1, Acc) of
		{Name, Pkgs} ->
			Intersection = [P || P <- Pkgs
				, lists:keymember(edm_pkg:get(version, P), VsnPos, Pkgs)],

			NewAcc = case Intersection of
				[] ->
					edm_log:error(
						  "Incompatible dependencies for ~p (~p not in ~p)"
						, [Name, Pkgs, Match]),
					error({depconflict, Name, Pkgs, Match});
				Intersection ->
					lists:keystore(Name, 1, Acc, {Name, Intersection})

			end,
			reduce_pkg_deps(Tail, NewAcc);
		false ->
			reduce_pkg_deps(Tail, [{Name, Match} | Acc])
	end.

%reduce_pkg_deps([{Name, Pkgs} | Tail], Acc) ->
%	case lists:keyfind(Name, 1, Acc) of
%		false ->
%			reduce_pkg_deps(Tail, [Pkg | Acc]);
%		Pkg2 ->
%			{Vsn, Vsn2} =
%				{edm_pkg:get(version, Pkg), edm_pkg:get(version, Pkg2)},
%			if
%				Vsn =:= Vsn2 ->
%					edm_log:debug("vsn match, reducing ~p from deplist", [Pkg2]),
%					reduce_pkg_deps(Tail, [Pkg | Acc]);
%				true ->
%					edm_log:error("could not match dep vsn ~p, ~p", [Pkg, Pkg2]),
%					error({vsn_mismatch, Pkg, Pkg2})
%			end
%	end.

iter(Rec, '_') ->
	iter(Rec, resolved);
iter(#'edm_cfg.cfg'{} = Cfg, unresolved) ->
	{_Resolved, Unresolved} = resolve(Cfg),
	Unresolved;
iter(#'edm_cfg.cfg'{} = Cfg, resolved) ->
	{Resolved, _Unresolved} = resolve(Cfg),
	lists:keysort(edm_pkg:key(name), Resolved);
iter(#'edm_cfg.cfg'{deps = G}, deps) ->
	lists:map(fun(E) ->
		{E, _V1, V2, {Const, Opts}} = digraph:edge(G, E),
		{V2, Const, Opts}
	end, digraph:edges(G));
iter(#'edm_cfg.cfg'{opts = R}, opts) ->
	R;
iter(#'edm_cfg.cfg'{catalogs = R}, catalogs) ->
	R.
