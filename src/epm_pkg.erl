-module(epm_pkg).

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
	]).

-export([
	  select/2
	, foreach/2
	, map/2
	, foldl/3
	, foldr/3
	, flatten/1
	, keymerge/2
	, mergedeps/2
	, pathlens/1
	, pathlens/2
	]).

%% absname :: the absolute name specifying dependency tree
%% pkgname :: the actual package name used for catalog lookup
%% version :: the assumed version of the package to checkout
%% catalog :: list of catalog names where the pkg can be found
%% deps :: list of dependency packages
%% agent :: the agent that can be used to fetch pkg @todo corelate with catalog
%% path :: the checked out path of the pkg

-record(pkg, {
	  name :: binary()
	, absname = [] :: [binary()]
	, pkgname :: binary()
	, version = any :: binary() | any
	, catalog = [] :: [epm_catalog:catalog()]
	, deps = [] :: [pkg()]
	, agent = {epm_agent_git, []} :: {epm:agent(), AgentOpts :: [term()]}
	, synced = false :: true | false | error
	, path :: file:filename()
	, cfghook = fun(Cfg) -> Cfg end :: fun((epm:cfg()) -> epm:cfg())
	, isolate :: false | file:filename_all()
	}).

-opaque pkg() :: #pkg{}.

-type pkg_attr() :: name | absname | version  | catalog | deps | agent
	| {agent, atom()} | synced | path.

-type loopfun(Ret) :: fun((pkg()) -> Ret).
-type acc_loopfun(Ret) :: fun((pkg(), Ret) -> Ret).
-type traversable() :: epm:cfg() | [pkg()].

-export_type([pkg/0]).

-spec new(binary()) -> #pkg{}.
new(<<"_root">>) ->
	#pkg{name = <<"_root">>};
new(Name) ->
	new(Name, []).

-spec new(binary(), Attrs :: [{pkg_attr(), term()}]) -> #pkg{}.
new(Name, Attrs) ->
	Pkg = lists:foldl(fun({K, V}, Acc) ->
		set(K, V, Acc)
	end, #pkg{name = Name, pkgname = Name, absname = [Name]}, Attrs),
	((get(agent, Pkg))):init(Pkg).

-spec fetch(pkg(), epm:cfg()) -> ok | {error, Reason :: term()}.
fetch(#pkg{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:fetch(Pkg, Cfg).

-spec sync(pkg(), epm:cfg()) -> ok | {error, Reason :: term()}.
sync(#pkg{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:sync(Pkg, Cfg).

-spec status(pkg(), epm:cfg()) -> ok | stale | {error, Reason :: term()}.
status(#pkg{agent = {Agent,_}} = Pkg, Cfg) ->
	Agent:status(Pkg, Cfg).

-spec get(pkg_attr() | [pkg_attr()], pkg()) -> [term()].
get(Attrs, Pkg) when is_list(Attrs) ->
	[get(A, Pkg) || A <- Attrs];
get(Attr, Pkg) ->
	get_(Attr, Pkg).

get_(name, Pkg) -> Pkg#pkg.name;
get_(absname, Pkg) -> Pkg#pkg.absname;
get_(pkgname, Pkg) -> Pkg#pkg.pkgname;
get_(version, Pkg) -> Pkg#pkg.version;
get_(catalog, Pkg) -> Pkg#pkg.catalog;
get_(deps, Pkg) -> Pkg#pkg.deps;
get_({dep, Path}, Pkg) ->
	{G, _S} = pathlens(Path, pkg),
	G(Pkg);
get_(agent, #pkg{agent = {Agent, _}}) -> Agent;
get_({agent, opts}, #pkg{agent = {_, Opts}}) ->
	Opts;
get_({agent, Opt}, #pkg{agent = {_, Opts}}) ->
	case lists:keyfind(Opt, 1, Opts) of
		{Opt, Val} ->
			Val;
		false ->
			undefined
	end;
get_(synced, Pkg) -> Pkg#pkg.synced;
get_(path, Pkg) -> Pkg#pkg.path;
get_(cfghook, Pkg) -> Pkg#pkg.cfghook.

-spec set(pkg_attr(), term(), epm:cfg()) -> none().
set(name, Val, Pkg) -> Pkg#pkg{name = Val};
set(absname, Val, Pkg) -> Pkg#pkg{absname = Val};
set(pkgname, Val, Pkg) -> Pkg#pkg{pkgname = Val};
set(version, Val, Pkg) -> Pkg#pkg{version = Val};
set(catalog, Val, Pkg) -> Pkg#pkg{catalog = Val};
set(deps, Val, Pkg) -> Pkg#pkg{deps = Val};
set({dep, Path}, Val, Pkg) ->
	{_G, S} = pathlens(Path, pkg),
	S(Val, Pkg);
set(agent, Val, Pkg) -> Pkg#pkg{agent = {Val, []}};
set({agent, K}, Val, #pkg{ agent = {B, O}} = Pkg) ->
	Pkg#pkg{agent = {B, [{K, Val} | O]}};
set(synced, Val, Pkg) -> Pkg#pkg{synced = Val};
set(path, Val, Pkg) -> Pkg#pkg{path = Val};
set(cfghook, Val, Pkg) -> Pkg#pkg{cfghook = Val}.

-spec select({atom(), binary()}, traversable()) -> [pkg()].
select(Selector, Cfg) when is_tuple(Cfg) ->
	select(Selector, epm:get(deps, Cfg));
select({name, Name}, Iter) ->
	[ X || #pkg{name = M} = X <- Iter, Name == M].

-spec foreach(loopfun(Ret), traversable()) -> Ret when Ret :: none().
foreach(Fun, Iter) when is_list(Iter) ->
	lists:foreach(Fun, Iter);
foreach(Fun, #pkg{} = Pkg) ->
	lists:foreach(Fun, ?MODULE:get(deps, Pkg));
foreach(Fun, Cfg) when is_tuple(Cfg) ->
	lists:foreach(Fun, epm:get(deps, Cfg)).

-spec map(loopfun(Ret), traversable()) -> [Ret] when Ret :: pkg().
map(Fun, Iter) when is_list(Iter) ->
	lists:map(Fun, Iter);
map(Fun, #pkg{} = Pkg) ->
	lists:map(Fun, ?MODULE:get(deps, Pkg));
map(Fun, Cfg) when is_tuple(Cfg) ->
	lists:map(Fun, epm:get(deps, Cfg)).

-spec foldl(acc_loopfun(Ret), Ret, traversable()) -> Ret when Ret :: pkg().
foldl(Fun, Acc0, Iter) when is_list(Iter) ->
	lists:foldl(Fun, Acc0, Iter);
foldl(Fun, Acc0, #pkg{} = Pkg) ->
	lists:foldl(Fun, Acc0, ?MODULE:get(deps, Pkg));
foldl(Fun, Acc0, Cfg) when is_tuple(Cfg) ->
	lists:foldl(Fun, Acc0, epm:get(deps, Cfg)).

-spec foldr(acc_loopfun(Ret), Ret, traversable()) -> Ret when Ret :: pkg().
foldr(Fun, Acc0, Iter) when is_list(Iter) ->
	lists:foldr(Fun, Acc0, Iter);
foldr(Fun, Acc0, #pkg{} = Pkg) ->
	lists:foldr(Fun, Acc0, ?MODULE:get(deps, Pkg));
foldr(Fun, Acc0, Cfg) when is_tuple(Cfg) ->
	lists:foldr(Fun, Acc0, epm:get(deps, Cfg)).

-spec flatten(traversable(), [pkg()]) -> traversable().
flatten(#pkg{deps = Iter}) ->
	flatten(Iter, []);
flatten(Iter) when is_tuple(Iter) ->
	flatten(epm:get(deps, Iter), []);
flatten(Iter) ->
	flatten(Iter, []).

flatten([], Acc) ->
	lists:reverse(Acc);
flatten([Pkg | Iter], Acc) when is_list(Iter) ->
	Deps = [Pkg | flatten(Pkg)],
	flatten(Iter, Deps ++ Acc).

-spec keymerge(A, A) -> A when A :: [#pkg{}].
keymerge(A, B) ->
	lists:ukeymerge(#pkg.name
	, lists:ukeysort(#pkg.name, A)
	, lists:ukeysort(#pkg.name, B)).

-spec mergedeps(A, A) -> A when A :: [#pkg{}].
mergedeps(#pkg{deps = A}, #pkg{deps = B} = Pkg) ->
	Pkg#pkg{deps = keymerge(A, B)}.

-spec pathlens(Path) -> {Getter :: fun((Cfg) -> Pkg),
                         Setter :: fun((Pkg, Cfg) -> Cfg)}
                         when Path :: [binary()]
                            , Pkg:: pkg()
                            , Cfg :: epm:pkg().
pathlens(Path) ->
	pathlens(Path, cfg).

pathlens(Path, Type) ->
	compose(buildlens(Path, Type)).

buildlens(Path, cfg) ->
	[access_e(3) | buildlens(Path, list)]; %% 3 = deps in epm:#cfg{}
buildlens(Path, pkg) ->
	[access_e(#pkg.deps) | buildlens(Path, list)];
buildlens(Path, list) ->
	[_ | Lens] = lists:foldr(fun(P, Acc) ->
		[access_e(#pkg.deps), access_p(P, #pkg.name) | Acc]
	end, [], Path),
	Lens.

access_e(N) ->
	{fun(R)    -> element(N, R) end,
	 fun(A, R) -> setelement(N, R, A) end}.

%% Turns out that a proplist is also a lens.
%access_p(K) ->
%	access_p(K, 1).

access_p(K, N) ->
	{ fun(R)    ->
		case lists:keyfind(K, N, R) of
			{K, V} -> V;
			V -> V
		end end
	 , fun(A, R) -> lists:keystore(K, N, R, A) end}.

compose({LG, LP}, {KG, KP}) ->
	{fun(R)    -> KG(LG(R)) end,
	 fun(A, R) -> LP(KP(A, LG(R)), R) end}.

compose(Lenses) when is_list(Lenses) ->
	IdLens = {fun(X) -> X end, fun(A, _X) -> A end},
    lists:foldr(fun compose/2, IdLens, Lenses).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pathlens_test() ->
	Pkgs0 = [
		  [<<"a1">>,<<"b1">>, <<"c1">>]
		, [<<"a2">>, <<"b2">>]
		],

	GenPath = fun(P, Acc) ->
		new(P, [{deps, [Acc]}])
	end,
	[A1, A2] = Pkgs = [lists:foldr(GenPath, [], P) || P <- Pkgs0],
	[B1] = get(deps, A1),
	[C1] = get(deps, B1),
	[B2] = get(deps, A2),
	Cfg = epm:new([{deps, Pkgs}]),
	Match = [ {A1, [<<"a1">>]}, {B1, [<<"a1">>, <<"b1">>]}
		, {C1, [<<"a1">>, <<"b1">>, <<"c1">>]}, {A2, [<<"a2">>]}
		, {B2, [<<"a2">>, <<"b2">>]}],
	RootPkg = new(<<"root-pkg">>, [{deps, Pkgs}]),
	lists:foreach(fun({M, P}) ->
		{CG, CS} = pathlens(P, cfg),
		{PG, PS} = pathlens(P, pkg),
		?assertEqual(M, CG(Cfg)),
		?assertEqual(M, PG(RootPkg)),
		M2 = set(path, <<"abcd">>, M),
		?assertEqual(M2, CG(CS(M2, Cfg))),
		?assertEqual(M2, PG(PS(M2, RootPkg)))
	end, Match).


pathlens_r([], Acc) ->
	Acc;
pathlens_r([P | T] = A, Acc) when is_list(A), is_binary(P) ->
	pathlens_r(T, new(P));
pathlens_r([P | T] = A, Acc) when is_list(A), is_list(P) ->
	set(  deps
		, keymerge(
			  get(deps, lists:foldl(fun pathlens_r/2, #pkg{}, P))
			, get(deps, Acc))
		, Acc);
pathlens_r(P, Acc) when is_binary(P) ->
	set(deps
		, keymerge([new(P)], get(deps, Acc))
		, Acc).

-endif.
