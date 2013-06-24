-module(edm_catalog_mem).

-export([
	  name/0
	, match/1
	, fetch/1
	, resource/2
	]).

resource(Pkg, _Ctl) ->
	edm_pkg:get([name, version, path], Pkg).

name() ->
	<<"memory">>.

match(mem) -> true;
match(_)   -> false.

fetch(Ctl0) ->
	%% Provide a unique resource identifier for memory catalogs
	Ref = base64:encode(crypto:sha(term_to_binary(make_ref()))),
	Ctl = edm_cat:set(resource, binary:part(Ref, {0, 7}), Ctl0),
	{ok, Ctl}.
