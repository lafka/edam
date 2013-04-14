-module(epm).

-export([
	  init/0
	, init/1
	, new/0
	, new/1
	, parse/0
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

-record(cfg, {
	  catalogs = [] :: [epm_catalog:catalog()]
	, deps = [] :: [epm_pkg:pkg()]
	, opts = [] :: [{atom(), term()}]
	, callbacks = [] :: [fun((cfg()) -> cfg())]
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

-spec parse() -> #cfg{}.
parse() ->
	parse(".").

-spec parse(file:filename()) -> #cfg{}.
parse(Path) when is_list(Path) ->
	{ok, Cfg0} = parse(Path, epm_pkg:new(<<"_root">>)),
	Cfg = lists:foldl(fun(Fun, Acc) ->
		Fun(Acc)
	end, Cfg0, Cfg0#cfg.callbacks),
	{ok, Cfg}.

-spec parse(file:filename(), epm_pkg:pkg()) -> #cfg{}.
parse(Path, Pkg) ->
	parse(Path, Pkg, #cfg{}).

parse(Path, Pkg, Cfg) when is_binary(Path) ->
	parse(binary_to_list(Path), Pkg, Cfg);
parse(Path, Pkg, Cfg0) ->
	Cfg = do_parse([epm_parser_conf, epm_parser_rebar], {Path, Pkg, Cfg0}),
	{ok, Cfg}.

do_parse([], {_Path, _Pkg, Acc}) ->
	Acc;
do_parse([Parser | T], {Path, Pkg, Cfg0} = Acc) ->
	Exists = filelib:is_dir(Path),
	AutoFetch = env(autofetch),

	case Parser:parse(Path, Pkg) of
		{Ctls, Deps} ->
			Cfg1 = set(catalogs, epm_catalog:keymerge(Ctls, get(catalogs, Cfg0)), Cfg0),
			Cfg2 = merge(Ctls, Deps, Pkg, Cfg1),
			do_parse(T, {Path, Pkg, Cfg2});
		false when AutoFetch and (not Exists)->
			ok = epm_pkg:sync(epm_pkg:set(path, Path, Pkg), Cfg0),
			do_parse(T, Acc);
		false ->
			do_parse(T, Acc)
	end.

merge(Ctls0, Deps0, Pkg, Cfg0) ->
	{Ctls1, Deps1, Cfg1} = fold_deps(Ctls0, Deps0, Cfg0),
	Callbacks = [epm_pkg:get(cfghook, Pkg) | Cfg1#cfg.callbacks],
	Deps = Deps1,
	Cfg0#cfg{
		  deps = epm_pkg:keymerge(Deps, get(deps, Cfg0))
		, callbacks = Callbacks
		, catalogs = epm_catalog:keymerge(Ctls1, Cfg0#cfg.catalogs)}.

fold_deps(Ctls, Deps, Cfg) ->
	epm_pkg:foldl(fun fold_dep2/2, {Ctls, [], Cfg}, Deps).

fold_dep2(Dep, {Ctls0, Deps0, Cfg0}) ->
	[Name, Version0] = epm_pkg:get([name, version], Dep),
	DepDir = case {env(append_versions, true), Version0} of
		{true, any} -> Name;
		{true, _} -> <<Name/binary, $-, Version0/binary>>;
		{false, _} -> Name end,
	CfgFile = filename:join([epm:env(libdir), DepDir]),
	case parse(CfgFile, Dep, Cfg0#cfg{deps = []}) of
		{ok, Cfg} when Cfg =:= #cfg{} ->
			{Ctls0, [Dep | Deps0], Cfg};
		{ok, Cfg} ->
			[Ctls1, Deps1] = get([catalogs, deps], Cfg),
			{ Ctls1
			, [epm_pkg:set(deps, Deps1, Dep) | Deps0]
			, Cfg}
	end.

-spec get(atom(), cfg()) -> term().
get(Attrs, Cfg) when is_list(Attrs) ->
	[get(Attr, Cfg) || Attr <- Attrs];
get(catalogs, Cfg) -> Cfg#cfg.catalogs;
get(deps, Cfg) -> Cfg#cfg.deps;
get({dep, Path}, Cfg) ->
	{G, _S} = epm_pkg:pathlens(Path),
	G(Cfg);
get(opts, Cfg) -> Cfg#cfg.opts;
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
set(deps, Val, Cfg) -> Cfg#cfg{deps = Val};
set({dep, Path}, Val, Cfg) ->
	{_G, S} = epm_pkg:pathlens(Path),
	S(Val, Cfg);
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
