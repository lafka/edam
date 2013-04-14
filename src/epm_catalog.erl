-module(epm_catalog).

-export([
	  catalogs/0
	, new/2
	, get/2
	, set/3
	, keymerge/2
	]).

-export([
	  select/2
	, foreach/2
	, map/2
	, foldl/3
	, foldr/3
	]).

-record(catalog, {
	  name :: binary() | undefined
	, resource :: binary()
	, opts :: [{atom(), term()}]
	, module :: module()
	, pkgs = [] :: [epm_pkg:pkg()]
	}).

-opaque catalog() :: #catalog{}.

-type loopfun(Ret) :: fun((catalog()) -> Ret).
-type acc_loopfun(Ret) :: fun((catalog(), Ret) -> Ret).
-type traversable() :: epm:cfg() | [catalog()].

-spec catalogs() -> [module()].
catalogs() ->
	[epm_catalog_github].

-spec new(binary(), binary()) -> {ok, catalog()} | false.
new(Name, Resource) ->
	new(Name, Resource, catalogs()).

new(_Name, _Resource, []) ->
	false;
new(Name, Resource, [CtMod| T]) ->
	case CtMod:match(Resource) of
		true ->
			Ctl = #catalog{name = Name
				, resource = Resource
				, module = CtMod},
			CtMod:fetch(Ctl);
		false ->
			new(Name, Resource, T)
	end.

-spec get(atom() | [atom()], #catalog{}) -> term().
get(Attrs, Ctl) when is_list(Attrs) ->
	[get(Attr, Ctl) || Attr <- Attrs];
get(name, Ctl) -> Ctl#catalog.name;
get(resource, Ctl) -> Ctl#catalog.resource;
get(module, Ctl) -> Ctl#catalog.module;
get(pkgs, Ctl) -> Ctl#catalog.pkgs;
get({pkg, PkgName}, Ctl) ->
	epm_pkg:select({name, PkgName}, Ctl#catalog.pkgs);
get({opt, Opt}, #catalog{opts = Opts}) ->
	case lists:keyfind(Opt, 1, Opts) of
		{Opt, Val} ->
			Val;
		false ->
			undefined
	end.

-spec set(atom(), term(), #catalog{}) -> #catalog{}.
set(name, Val, Ctl) -> Ctl#catalog{name = Val};
set(resource, Val, Ctl) -> Ctl#catalog{resource = Val};
%set(module, Val, Ctl) -> Ctl#catalog{ = Val};
set(pkgs, Val, Ctl) -> Ctl#catalog{pkgs = Val};
set(opt, Val, #catalog{opts= Opts} = Ctl) ->
	Ctl#catalog{opts = [Val|Opts]}.

-spec select({atom(), binary()}, epm:cfg()) -> [catalog()].
select({name, Name}, Cfg) ->
	[ X || #catalog{name = M} = X <- epm:get(catalogs, Cfg), M == Name];
select({resource, Res}, Cfg) ->
	[ X || #catalog{resource = M} = X <- epm:get(catalogs, Cfg), M == Res];
select({pkg, Name}, Cfg) ->
	foldl(fun(Ct, Acc) ->
		case epm_pkg:select({name, Name}, Ct#catalog.pkgs) of
			[] ->
				Acc;
			[_ | _] ->
				[Ct | Acc]
		end
	end, [], Cfg).

-spec foreach(loopfun(Ret), traversable()) -> Ret when Ret :: none().
foreach(Fun, Iter) when Iter ->
	lists:foreach(Fun, Iter);
foreach(Fun, Iter) ->
	lists:foreach(Fun, epm:get(catalogs, Iter)).

-spec map(loopfun(Ret), traversable()) -> [Ret] when Ret :: catalog().
map(Fun, Iter) when is_list(Iter) ->
	lists:map(Fun, Iter);
map(Fun, Iter) ->
	lists:map(Fun, epm:get(catalogs, Iter)).

-spec foldl(acc_loopfun(Ret), Ret, traversable()) -> Ret when Ret :: catalog().
foldl(Fun, Acc0, Iter) when is_list(Iter) ->
	lists:foldl(Fun, Acc0, Iter);
foldl(Fun, Acc0, Iter) ->
	lists:foldl(Fun, Acc0, epm:get(catalogs, Iter)).

-spec foldr(acc_loopfun(Ret), Ret, traversable()) -> Ret when Ret :: catalog().
foldr(Fun, Acc0, Iter) when is_list(Iter) ->
	lists:foldr(Fun, Acc0, Iter);
foldr(Fun, Acc0, Iter) ->
	lists:foldr(Fun, Acc0, epm:get(catalogs, Iter)).

-spec keymerge(A, A) -> A when A :: [#catalog{}].
keymerge(A, B) ->
	lists:ukeymerge(#catalog.resource
	, lists:ukeysort(#catalog.resource, A)
	, lists:ukeysort(#catalog.resource, B)).
