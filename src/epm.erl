-module(epm).

-export([
	  init/0
	, init/1
	, new/0
	, new/1
	, parse/1
	, parse/2
	, get/2
	, set/3
	, env/1
	, env/2
	, set_env/2
	, unless/2
	, unless/3
	, ifthen/2
	, ifthen/3
	, log/2
	, log/3
	]).

-define(defaultenv, [
	  {autofetch, true}
	, {cachedir, ".cache"}
	, {dryrun, false}
	, {libdir, "lib"}
	]).

-type key() :: atom() | tuple(atom()).
-type absname() :: [binary()].
-type partial() :: [{key(), term()}].
-type partials() :: [{absname(), partial()}].

%% catalogs :: list of catalog records
%% pkgs :: list of packages related to this configuration
%% callbacks :: list of fun((Cfg) -> Cfg) callbacks that runs after parsing
%% root :: the absolute path of this configuration
%% isolate :: flag to indicate a standalone configuration
%% templates :: list of partials applied before pkg construction
%% partials :: list of partials applied after pkg construction
-record(cfg, {
	  catalogs = [] :: [epm_catalog:catalog()]
	, pkgs = [] :: [epm_pkg:pkg()]
	, opts = [] :: [{atom(), term()}]
	, callbacks = [] :: [fun((cfg()) -> cfg())]
	, root :: file:filename_all()
	, isolate = false :: boolean()
	, templates = [] :: partials()
	, partials = [] :: partials()
	}).

-opaque cfg() :: #cfg{}.

-export_type([cfg/0]).

init() ->
	init([]).

init(Args) ->
	[set_env(K, V) || {K, V} <- lists:ukeymerge(1, Args, ?defaultenv)].

new() ->
	new([]).

new(Opts) ->
	lists:foldl(fun({K, V}, Cfg) -> set(K, V, Cfg) end, #cfg{}, Opts).

-spec parse(file:filename_all()) -> {ok, #cfg{}}.
parse(Path) when is_list(Path) ->
	parse(list_to_binary(Path));
parse(Path) when is_binary(Path) ->
	RootName = list_to_binary(filename:basename(filename:absname("./"))),
	PkgOpts = [{isolate, Path}, {agent, epm_agent_void}],
	parse(Path, epm_pkg:new(RootName, PkgOpts)).

-spec parse(binary(), Parent :: epm_pkg:pkg()) -> {ok, #cfg{}}.
parse(Path, Pkg) ->
	AbsName = epm_pkg:get(absname, Pkg),

	AutoFetch = env(autofetch),
	Exists = filelib:is_dir(Path),

	if
		not Exists and AutoFetch->
			{ok, Cfg} = epm_store:get(AbsName),
			epm_pkg:sync(Pkg, Cfg);
		true -> ok end,

	lists:foreach(fun(Parser) ->
		Parser:parse(Path, Pkg)
	end, [epm_parser_conf, epm_parser_rebar]),

	Pkg1 = epm_store:get(AbsName, {pkg, epm_pkg:get(absname, Pkg)}),
	epm_pkg:foreach(fun(Dep) ->
		parse(epm_pkg:get(path, Dep), Dep)
	end, Pkg1),

	epm_store:get(AbsName).

-spec get(atom(), cfg()) -> term().
get(Attrs, Cfg) when is_list(Attrs) ->
	[get(Attr, Cfg) || Attr <- Attrs];
get(catalogs, Cfg) -> Cfg#cfg.catalogs;
get({catalog, Ctl}, Cfg) ->
	case epm_catalog:select({name, Ctl}, Cfg) of
		[Catalog] ->
			Catalog;
		[] ->
			false
	end;
get(deps, Cfg) -> Cfg#cfg.pkgs;
get(pkgs, Cfg) -> Cfg#cfg.pkgs;
get({dep, Path}, Cfg) ->
	get({pkg, Path}, Cfg);
get({pkg, Path}, Cfg) ->
	{G, _S} = epm_pkg:pathlens(Path),
	G(Cfg);
get(opts, Cfg) -> Cfg#cfg.opts;
get(root, Cfg) -> Cfg#cfg.root;
get(partials, Cfg) -> Cfg#cfg.partials;
get({partial, AbsName}, Cfg) ->
	case lists:keyfind(AbsName, 1, Cfg#cfg.partials) of
		false -> [];
		{AbsName, Partial} -> Partial
	end;
get({opt, Key}, Cfg) ->
	case lists:keyfind(Key, 1, Cfg#cfg.opts) of
		{Key, Val} ->
			Val;
		false ->
			undefined
	end;
get(callbacks, Cfg) ->
	Cfg#cfg.callbacks.

-spec set(atom(), Val, cfg()) -> Val when Val :: term().
set(catalogs, Val, Cfg) -> Cfg#cfg{catalogs = Val};
set(catalog,  Val, Cfg) ->
	Cfg#cfg{catalogs = epm_catalog:keymerge([Val], Cfg#cfg.catalogs)};
set(deps, Val, Cfg) -> Cfg#cfg{pkgs = Val};
set(pkgs, Val, Cfg) -> Cfg#cfg{pkgs = Val};
set({dep, AbsName}, Val, Cfg) ->
	set({pkg, AbsName}, Val, Cfg);
set({pkg, [_Name]}, Val, Cfg) ->
	set(pkgs, epm_pkg:keymerge([Val], Cfg#cfg.pkgs), Cfg);
set({pkg, AbsName}, Val, Cfg) ->
	{_G, S} = epm_pkg:pathlens(AbsName),
	S(Val, Cfg);
set({partial, AbsName}, Val, #cfg{partials = Parts} = Cfg) ->
	Partial = case lists:keyfind(AbsName, 1, Parts) of
		false ->
			lists:ukeysort(1, Val);
		{AbsName, Partial0} ->
			lists:ukeymerge(1, lists:ukeysort(Val), Partial0)
	end,
	Cfg#cfg{
		partials = lists:keystore(AbsName, 1, Parts, {AbsName, Partial})};
set(root, Val, Cfg) -> Cfg#cfg{root = Val};
set({opt, K}, Val, Cfg) ->
	Cfg#cfg{opts = lists:ukeymerge(1, [{K, Val}], Cfg#cfg.opts)}.

-spec unless(atom(), fun(() -> Ret)) -> Ret when Ret :: term().
unless(Opt, Fun) ->
	unless(Opt, Fun, fun() -> ok end).

-spec unless(atom(), fun(() -> Ret), fun(() -> Ret)) -> Ret when Ret :: term().
unless(Opt, A, B) ->
	case ?MODULE:env(Opt) of
		true ->
			B();
		false ->
			A()
	end.

-spec ifthen(atom(), fun(() -> Ret)) -> Ret when Ret :: term().
ifthen(Opt, Fun) ->
	ifthen(Opt, Fun, fun() -> ok end).

-spec ifthen(atom(), fun(() -> Ret), fun(() -> Ret)) -> Ret when Ret :: term().
ifthen(Opt, A, B) ->
	case ?MODULE:env(Opt) of
		true ->
			A();
		false ->
			B()
	end.

-spec env(atom()) -> term().
env(Key) ->
	case application:get_env(epm, Key) of
		{ok, Val} ->
			Val;
		undefined ->
			error(badarg, [Key])
	end.

-spec env(atom(), Default :: term()) -> term().
env(Key, Default) ->
	case application:get_env(epm, Key) of
		{ok, Val} ->
			Val;
		undefined ->
			Default
	end.

-spec set_env(atom(), Val) -> Val when Val :: term().
set_env(Key, Val) ->
	application:set_env(epm, Key, Val),
	Val.

-spec log(atom(), string()) -> none().
log(Level, Msg) ->
	log(Level, Msg, []).

-spec log(atom(), string(), [term()]) -> none().
log(Level, Msg, Args) ->
	case {match_level(env(log_level, debug)), match_level(Level)} of
		{L0, L1} when L0 >= L1 ->
			{_, {H, M, S}} = erlang:universaltime(),
			Level2 = "[" ++ atom_to_list(Level) ++ "]",
			io:format("~10s ~2..0b:~2..0b:~2..0b -> " ++ Msg ++ "~n"
				, [Level2, H, M, S] ++ Args);
		{_, _} ->
			ok
	end.

match_level(command) -> 0;
match_level(error) -> 0;
match_level(warning) -> 1;
match_level(info) -> 2;
match_level(notice) -> 3;
match_level(debug) -> 4.
