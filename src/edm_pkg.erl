-module(edm_pkg).

-export([
	  new/1
	, new/2
	, fetch/2
	, sync/2
	, status/2
	]).

-export([
	  get/2
	, set/3
	, iter/2
	]).

-record('edm_pkg.pkg', {
	  name :: name()
	, absname = [] :: [binary()]
	, pkgname :: binary()
	, version = any :: vsn()
	, catalog = [] :: [edm_cat:cat()]
	, deps = [] :: [pkg()]
	, agent = {dagent_git, []} :: {edam:agent(), AgentOpts :: [term()]}
	, path :: file:filename()
	, isolate = false :: false | file:filename_all()
	, template = false :: boolean()
	}).

-opaque pkg() :: #'edm_pkg.pkg'{}.

-type name() :: binary().
-type vsn() :: binary() | any.
-type pkgattrs() :: name | absname | pkgname | version | catalog | deps |
                    agent | {agent, atom()}| path | isolate | template.

-export_type([pkg/0]).

-spec new(binary()) -> #'edm_pkg.pkg'{}.
new(Name) ->
	new(Name, []).

-spec new(binary(), Attrs :: [{pkgattrs(), term()}]) -> #'edm_pkg.pkg'{}.
new(Name, Attrs0) ->
	BaseAttrs = [{absname, [Name]}, {name, Name}, {pkgname, Name}],
	Attrs = lists:ukeymerge(1
		, lists:ukeysort(1, proplists:unfold(Attrs0))
		, BaseAttrs),

	AbsName  = proplists:get_value(absname, Attrs),

	Pkg0 = lists:foldl(fun
		({K, V}, Acc) -> set(K, V, Acc)
	end, #'edm_pkg.pkg'{}, Attrs),

	case get([isolate, absname], Pkg0) of
		[Root, AbsName] when is_binary(Root) ->
			dstore:new(AbsName, [{root, Root}]);
		_ ->
			ok
	end.

-spec fetch(pkg(), disli:cfg()) -> ok | {error, Reason :: term()}.
fetch(#'edm_pkg.pkg'{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:fetch(Pkg, Cfg).

-spec sync(pkg(), disli:cfg()) -> ok | {error, Reason :: term()}.
sync(#'edm_pkg.pkg'{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:sync(Pkg, Cfg).

-spec status(pkg(), disli:cfg()) -> ok | stale | {error, Reason :: term()}.
status(#'edm_pkg.pkg'{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:status(Pkg, Cfg).

-spec get(pkgattrs() | list(pkgattrs()), pkg()) -> [term()].
get(Attrs, Pkg) when is_list(Attrs) ->
	[get(Attr, Pkg) || Attr <- Attrs];
get(name, Pkg)              -> Pkg#'edm_pkg.pkg'.name;
get(absname, Pkg)           -> Pkg#'edm_pkg.pkg'.absname;
get(pkgname, Pkg)           -> Pkg#'edm_pkg.pkg'.pkgname;
get(version, Pkg)           -> Pkg#'edm_pkg.pkg'.version;
get(catalog, Pkg)           -> Pkg#'edm_pkg.pkg'.catalog;
get(isolate, Pkg)           -> Pkg#'edm_pkg.pkg'.isolate;
get(template, Pkg)          -> Pkg#'edm_pkg.pkg'.template;
get(deps, Pkg)              -> Pkg#'edm_pkg.pkg'.deps;
get(path, Pkg)              -> Pkg#'edm_pkg.pkg'.path;
get(agent, #'edm_pkg.pkg'{agent = {Agent, _}}) -> Agent;
get({agent, opts}, #'edm_pkg.pkg'{agent = {_, Opts}}) ->
	Opts;
get({agent, Opt}, #'edm_pkg.pkg'{agent = {_, Opts}}) ->
	case lists:keyfind(Opt, 1, Opts) of
		{Opt, Val} ->
			Val;
		false ->
			undefined
	end.

-spec set(pkgattrs(), term(), pkg()) -> none().
set(name, Val, Pkg)         -> Pkg#'edm_pkg.pkg'{name = Val};
set(absname, Val, Pkg)      -> Pkg#'edm_pkg.pkg'{absname = Val};
set(pkgname, Val, Pkg)      -> Pkg#'edm_pkg.pkg'{pkgname = Val};
set(version, Val, Pkg)      -> Pkg#'edm_pkg.pkg'{version = Val};
set(catalog, Val, Pkg)      -> Pkg#'edm_pkg.pkg'{catalog = Val};
set(isolate, Val, Pkg)      -> Pkg#'edm_pkg.pkg'{isolate = Val};
set(template, Val, Pkg)     -> Pkg#'edm_pkg.pkg'{template = Val};
set(deps, Val, Pkg)         -> Pkg#'edm_pkg.pkg'{deps = Val};
set(path, Val, Pkg)         -> Pkg#'edm_pkg.pkg'{path = Val};
set(agent, Val, Pkg)        -> Pkg#'edm_pkg.pkg'{agent = {Val, []}};
set({agent, K}, Val, #'edm_pkg.pkg'{agent = {Mod, Opts}} = Pkg) ->
	Opts = lists:keystore(K, 1, Opts, {K, Val}),
	Pkg#'edm_pkg.pkg'{agent = {Mod, Opts}}.

-spec iter(pkg(), '_' | deps) -> [pkg()].
iter(Rec, '_') ->
	iter(Rec, deps);
iter(#'edm_pkg.pkg'{deps = R}, deps) ->
	R.
