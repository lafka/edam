-module(edm_cat).

-export([
	  new/2
	, new/3
	, key/1
	, get/2
	, set/3
	, resolve/2
	]).

-record('edm_cat.ctl', {
	  name :: binary() | undefined
	, resource :: binary()
	, pkgopts = [] :: [{atom(), term()}] %% Options applied to pkg when resolved
	, module :: module()
	, pkgs = [] :: [edm_pkg:pkg()]
	}).

-opaque cat() :: #'edm_cat.ctl'{}.

-type catattrs() :: name | resource | module | pkgs | pkgopts | {pkgopt, atom()}.

-export_type([cat/0]).

-spec new(binary(), binary()) -> {ok, cat()} | false.
new(Name, Resource) ->
	new(Name, Resource, edm_env:get('catalogs')).

-spec new(binary(), binary(), [module()]) -> {ok, cat()} | false.
new(_Name, _Resource, []) ->
	false;
new(Name, Resource, [Mod | T]) ->
	case Mod:match(Resource) of
		true ->
			Ctl = #'edm_cat.ctl'{
				  name = Name
				, resource = Resource
				, module = Mod},
			Mod:fetch(Ctl);
		false ->
			new(Name, Resource, T)
	end.

-spec resolve(edm_pkg:constraint(), edm_cfg:cfg()) -> [edm_pkg:pkg()].

resolve({Name, Constraints, Opts}, Cfg) ->
	iter:foldl(fun(Cat, Acc) ->
		case get({pkg, Name}, Cat) of
			false -> false;
			Pkg0 ->
				case edm_pkg:constrained(Pkg0, Constraints) of
					true ->
						Acc;
					false ->
						CatOpts = get(pkgopts, Cat),
						Pkg = edm_pkg:set(CatOpts ++ Opts, Pkg0),

						[Pkg | Acc]
				end
		end
	end, [], Cfg, catalogs).

key(name) -> #'edm_cat.ctl'.name;
key(resource) -> #'edm_cat.ctl'.resource;
key(pkgopts) -> #'edm_cat.ctl'.pkgopts;
key(module) -> #'edm_cat.ctl'.module;
key(pkgs) -> #'edm_cat.ctl'.pkgs.


-spec get(catattrs() | [catattrs()], #'edm_cat.ctl'{}) -> term().
get(Attrs, Ctl) when is_list(Attrs) ->
	[get(Attr, Ctl) || Attr <- Attrs];
get({pkg, P}, Ctl) ->
	lists:keyfind(P, edm_pkg:key(name), Ctl#'edm_cat.ctl'.pkgs);
get({pkgopt, Opt}, #'edm_cat.ctl'{pkgopts = Opts}) ->
	case lists:keyfind(Opt, 1, Opts) of
		{Opt, Val} ->
			Val;
		false ->
			undefined
	end;
get(Key, #'edm_cat.ctl'{} = Pkg) when is_atom(Key) ->
	N = key(Key),
	erlang:element(N, Pkg).

-spec set(atom(), term(), #'edm_cat.ctl'{}) -> #'edm_cat.ctl'{}.
set({pkgopt, K}, Val, #'edm_cat.ctl'{pkgopts = Opts} = Ctl) ->
	Ctl#'edm_cat.ctl'{pkgopts = lists:keystore(K, 1, Opts, {K, Val})};
set(Key, Val, #'edm_cat.ctl'{} = Pkg) when is_atom(Key) ->
	N = key(Key),
	erlang:setelement(N, Pkg, Val).
