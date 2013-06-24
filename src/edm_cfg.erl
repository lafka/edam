-module(edm_cfg).

-compile([{no_auto_import, [get/1]}]).

-define(ref, edm_cfg_server).

-record('edm_cfg.cfg', {
	  catalogs = [] :: [edm_cat:cat()]
	, deps = [] :: [edm_pkg:constraint()]
	, resolved = [] :: [edm_pkg:pkg()]
	, opts = [] :: [{atom(), term()}]
	, path :: binary()
	}).

-opaque cfg() :: #'edm_cfg.cfg'{}.

-export_type([cfg/0]).

-export([
	  new/1
	, parse/1
	, parse/2
	, save/1
	, configs/0
	, key/1
	, get/2
	, set/3
	, iter/2
	]).

-spec new([{atom(), term()}]) -> cfg().
new(Opts) ->
	new(Opts, #'edm_cfg.cfg'{}).

%% @private create a new configuration based on Cfg
new(Opts0, Cfg) ->
	%% Make sure erl libs catalog is available
	Catalogs = proplists:append_values(catalogs, Opts0),
	LocalCat = lists:filter(fun filter_sys_libs/1, Catalogs),

	%% @todo olav 2013-06-24; Make option for not adding local erl libs
	Opts = if
		length(LocalCat) > 0 -> Opts0;
		true ->
			case edm_cat:new(erlang, list_to_binary(code:lib_dir())) of
				{ok, Cat0} ->
					Cat = edm_cat:set({pkgopt, state}, system, Cat0),
					edm_log:debug("appending local OTP libs to codepath (~s)", [code:lib_dir()]),
					[{catalogs, [Cat | Catalogs]}
						| proplists:delete(catalogs, Opts0)];
				false ->
					Opts0
			end
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

	NewCfg = lists:foldl(fun(P, Acc) ->
		case P:parse(Acc) of
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
key(deps) -> #'edm_cfg.cfg'.deps;
key(resolved) -> #'edm_cfg.cfg'.resolved;
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

iter(Rec, '_') ->
	iter(Rec, resolved);
iter(#'edm_cfg.cfg'{resolved = R}, resolved) ->
	R;
iter(#'edm_cfg.cfg'{deps = R}, deps) ->
	R;
iter(#'edm_cfg.cfg'{opts = R}, opts) ->
	R;
iter(#'edm_cfg.cfg'{catalogs = R}, catalogs) ->
	R.
