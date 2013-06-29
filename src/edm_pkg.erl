-module(edm_pkg).

-export([
	  new/1
	, new/2
	, fetch/2
	, sync/2
	, status/2
	]).

-export([
	  key/1
	, get/2
	, set/2
	, set/3
	, iter/2
	]).

-record('edm_pkg.pkg', {
	  name :: name()
	, pkgname :: binary()     % real package name, as given by publisher
	, canonical :: urn()      % canonical path e.g. "edam://pkg/<pkgname>/<vsn>
	, version = any :: vsn()
	, deps = [] :: [constraint()]
	, agent = {edm_agent_git, []} :: {edam:agent(), AgentOpts :: [term()]}
	, path :: file:filename()
	, state = unknown :: ok | error | unknown | ignore | system
	, template = false :: boolean()
	}).

-opaque pkg() :: #'edm_pkg.pkg'{}.

-type name() :: binary().
-type vsn() :: binary() | any.
-type attrs() :: name | pkgname | version | catalog | deps | agent |
                 {agent, atom()}| path | state | template.
-type constraint() :: {edm_pkg:name(),
	  [{edm_pkg:attrs(), term()}]
	, [{edm_pkg:attrs(), term()}]}.
-type urn() :: binary().


-export_type([pkg/0, name/0, vsn/0, attrs/0, constraint/0]).

key(name)         ->  #'edm_pkg.pkg'.name;
key(pkgname)      ->  #'edm_pkg.pkg'.pkgname;
key(canonical)    ->  #'edm_pkg.pkg'.canonical;
key(version)      ->  #'edm_pkg.pkg'.version;
key(deps)         ->  #'edm_pkg.pkg'.deps;
key(agent)        ->  #'edm_pkg.pkg'.agent;
key(path)         ->  #'edm_pkg.pkg'.path;
key(state)        ->  #'edm_pkg.pkg'.state;
key(template)     ->  #'edm_pkg.pkg'.template.

-spec new(binary()) -> #'edm_pkg.pkg'{}.
new(Name) ->
	new(Name, []).

-spec new(binary(), Attrs :: [{attrs(), term()}]) -> #'edm_pkg.pkg'{}.
new(Name, Attrs0) ->
	BaseAttrs = [{name, Name}, {pkgname, Name}],
	Attrs = lists:ukeymerge(1
		, lists:ukeysort(1, proplists:unfold(Attrs0))
		, BaseAttrs),

	lists:foldl(fun
		({K, V}, Acc) -> set(K, V, Acc)
	end, #'edm_pkg.pkg'{}, Attrs).

-spec fetch(pkg(), disli:cfg()) -> ok | {error, Reason :: term()}.
fetch(#'edm_pkg.pkg'{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:fetch(Pkg, Cfg).

-spec sync(pkg(), disli:cfg()) -> ok | {error, Reason :: term()}.
sync(#'edm_pkg.pkg'{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:sync(Pkg, Cfg).

-spec status(pkg(), disli:cfg()) -> ok | stale | {error, Reason :: term()}.
status(#'edm_pkg.pkg'{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:status(Pkg, Cfg).

-spec get(attrs() | list(attrs()), pkg()) -> [term()].
get(Attrs, Pkg) when is_list(Attrs) ->
	[get(Attr, Pkg) || Attr <- Attrs];
get(agent, #'edm_pkg.pkg'{agent = {Agent, _}}) -> Agent;
get({agent, opts}, #'edm_pkg.pkg'{agent = {_, Opts}}) ->
	Opts;
get({agent, Opt}, #'edm_pkg.pkg'{agent = {_, Opts}}) ->
	case lists:keyfind(Opt, 1, Opts) of
		{Opt, Val} ->
			Val;
		false ->
			undefined
	end;
get(Key, #'edm_pkg.pkg'{} = Pkg) when is_atom(Key) ->
	N = key(Key),
	erlang:element(N, Pkg).

-spec set(attrs(), term(), pkg()) -> none().
set(Attrs, Pkg) when is_list(Attrs) ->
	lists:foldl(fun({K, V}, Acc) -> set(K, V, Acc) end, Pkg, Attrs).

set(agent, Val, Pkg) -> Pkg#'edm_pkg.pkg'{agent = {Val, []}};
set({agent, K}, Val, #'edm_pkg.pkg'{agent = {Mod, Opts0}} = Pkg) ->
	Opts = lists:keystore(K, 1, Opts0, {K, Val}),
	Pkg#'edm_pkg.pkg'{agent = {Mod, Opts}};
set(version, Val, #'edm_pkg.pkg'{pkgname = PkgName} = Pkg) ->
	Pkg#'edm_pkg.pkg'{version = Val, canonical = urn(PkgName, Val)};
set(pkgname, Val, #'edm_pkg.pkg'{version = Vsn} = Pkg) ->
	Pkg#'edm_pkg.pkg'{pkgname = Val, canonical = urn(Val, Vsn)};
set(Key, Val, #'edm_pkg.pkg'{} = Pkg) when is_atom(Key) ->
	N = key(Key),
	erlang:setelement(N, Pkg, Val).

urn(Name, any) ->
	urn(Name, <<>>);
urn(Name, Vsn) ->
	Suffix = case Vsn of <<>> -> <<>>; Vsn -> <<"/", Vsn/binary>> end,
	<<"edam:pkg/", Name/binary, Suffix/binary>>.

%% @todo olav 2013-06-25; Add iterator that pre-resolves each dep
-spec iter(pkg(), '_' | deps) -> [pkg()].
iter(Rec, '_') ->
	iter(Rec, deps);
iter(#'edm_pkg.pkg'{deps = R}, deps) ->
	R.
