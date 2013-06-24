-module(edm_cat).

-export([
	  new/2
	, get/2
	, set/3
	]).

-record('edm_cat.ctl', {
	  name :: binary() | undefined
	, resource :: binary()
	, opts = [] :: [{atom(), term()}]
	, module :: module()
	, pkgs = [] :: [edm_pkg:pkg()]
	}).

-opaque cat() :: #'edm_cat.ctl'{}.

-type catattrs() :: name | resource | module | pkgs | opts | {opt, atom()}.

-export_type([cat/0]).

-spec new(binary(), binary()) -> {ok, cat()} | false.
new(Name, Resource) ->
	new(Name, Resource, edm_env:get('catalogs')).

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

-spec get(catattrs() | [catattrs()], #'edm_cat.ctl'{}) -> term().
get(Attrs, Ctl) when is_list(Attrs) ->
	[get(Attr, Ctl) || Attr <- Attrs];
get(name, Ctl)     -> Ctl#'edm_cat.ctl'.name;
get(resource, Ctl) -> Ctl#'edm_cat.ctl'.resource;
get(module, Ctl)   -> Ctl#'edm_cat.ctl'.module;
get(pkgs, Ctl)     -> Ctl#'edm_cat.ctl'.pkgs;
get(opts, Ctl)     -> Ctl#'edm_cat.ctl'.opts;
get({opt, Opt}, #'edm_cat.ctl'{opts = Opts}) ->
	case lists:keyfind(Opt, 1, Opts) of
		{Opt, Val} ->
			Val;
		false ->
			undefined
	end.

-spec set(atom(), term(), #'edm_cat.ctl'{}) -> #'edm_cat.ctl'{}.
set(name, Val, Ctl) -> Ctl#'edm_cat.ctl'{name = Val};
set(resource, Val, Ctl) -> Ctl#'edm_cat.ctl'{resource = Val};
set(pkgs, Val, Ctl) -> Ctl#'edm_cat.ctl'{pkgs = Val};
set({opt, K}, Val, #'edm_cat.ctl'{opts = Opts} = Ctl) ->
	Ctl#'edm_cat.ctl'{opts = lists:keystore(K, 1, Opts, {K, Val})}.
