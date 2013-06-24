-module(edm_cfg).

-compile([{no_auto_import, [get/1]}]).

-define(ref, edm_cfg_server).

-record('edm_cfg.cfg', {
	  catalogs = [] :: [edm_cat:cat()]
	, deps = [] :: [pkgconst()]
	, opts = [] :: [{atom(), term()}]
	, path :: binary()
	}).

-opaque cfg() :: #'edm_cfg.cfg'{}.

-type pkgconst() :: {edm_pkg:name(),
	  [{edm_pkg:attrs(), term()}]
	, [{edm_pkg:attrs(), term()}]}.

-export_type([cfg/0]).

-export([
	  new/1
	, parse/1
	, parse/2
	, save/1
	, configs/0
	, get/2
	, set/3
	, iter/2
	]).

-spec new([{atom(), term()}]) -> cfg().
new(Opts) ->
	new(Opts, #'edm_cfg.cfg'{}).

%% @private create a new configuration based on Cfg
new(Opts, Cfg) ->
	lists:foldl(fun({K, V}, Acc) ->
		set(K, V, Acc)
	end, Cfg, Opts).

parse(Path) ->
	parse(Path, []).

parse(Path0, Opts) ->
	Path = filename:absname(Path0),
	Cfg = new(Opts, #'edm_cfg.cfg'{path = Path}),
	save(Cfg),
	configs().

configs() ->
	gen_server:call(?ref, {config, list}).

-spec save(cfg()) -> ok.
save(Cfg) ->
	edm_cfg_server:persist(Cfg).

-spec get(atom(), cfg()) -> term().
get(Attrs, Cfg) when is_list(Attrs) ->
	[get(Attr, Cfg) || Attr <- Attrs];
get(catalogs, Cfg)        -> Cfg#'edm_cfg.cfg'.catalogs;
get(pkgs, Cfg)            -> Cfg#'edm_cfg.cfg'.pkgs;
get(path, Cfg)            -> Cfg#'edm_cfg.cfg'.path;
get(opts, Cfg)            -> Cfg#'edm_cfg.cfg'.opts;
get({opt, Key}, Cfg) ->
	case lists:keyfind(Key, 1, Cfg#'edm_cfg.cfg'.opts) of
		{Key, Val} ->
			Val;
		false ->
			undefined
	end.

-spec set(atom(), Val, cfg()) -> Val when Val :: term().
set(catalogs, Val, Cfg)    -> Cfg#'edm_cfg.cfg'{catalogs = Val};
set(pkgs, Val, Cfg)        -> Cfg#'edm_cfg.cfg'{pkgs = Val};
set(path, Val, Cfg)        -> Cfg#'edm_cfg.cfg'{path = Val};
set({opt, K}, Val, #'edm_cfg.cfg'{opts = Opts} = Cfg) ->
	Cfg#'edm_cfg.cfg'{opts = lists:keystore(K, 1, Opts, {K, Val})}.

iter(Rec, '_') ->
	iter(Rec, pkgs);
iter(#'edm_cfg.cfg'{pkgs = R}, pkgs) ->
	R;
iter(#'edm_cfg.cfg'{opts = R}, opts) ->
	R;
iter(#'edm_cfg.cfg'{catalogs = R}, catalogs) ->
	R.
