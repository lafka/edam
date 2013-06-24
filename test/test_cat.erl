-module(test_cat).

-export([
	  name/0
	, match/1
	, fetch/1
	, resource/2
	]).

resource(Pkg, _Ctl) ->
	<<"test://", ((edm_pkg:get(name, Pkg)))/binary>>.

name() ->
	<<"test">>.

match(<<"test://", _/binary>>) ->
	true;
match(_) ->
	false.

fetch(Ctl) ->
	Pkgs = [edm_pkg:new(<<"a">>), edm_pkg:new(<<"b">>)],
	{ok, edm_cat:set(pkgs, Pkgs, Ctl)}.
