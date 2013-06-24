-module(config_SUITE).

-export([
	  all/0
	, end_per_test/1
	, init_per_test/1
]).

-export([
	  config/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
	[config].

init_per_test(Cfg) ->
	{ok, Pid} = edm_cfg_server:start_link(),
	[{cfg_server, Pid} | Cfg].

end_per_test(Cfg) ->
	true = exit(?config(cfg_server), normal),
	Cfg.

config(_Cfg) ->
	edm_env:set(catalogs, [edm_catalog_local]),

	Path = filename:absname("./"),

	{ok, _Pid} = edm_cfg_server:start_link(),
	{ok, Cfg0} = edm_cfg_server:new([<<"test">>], [{path, Path}]),
	{ok, Cfg1} = edm_cfg_server:new([<<"test">>, <<"1">>], [{path, filename:join(Path, <<"1">>)}]),

	{ok, Cfg2} = edm_cfg_server:new([<<"test">>, <<"2">>], [{path, filename:join(Path, <<"2">>)}]),
	?assertEqual({ok, Cfg0}, edm_cfg_server:get([<<"test">>])),
	?assertEqual({ok, Cfg1}, edm_cfg_server:get([<<"test">>, <<"1">>])),
	?assertEqual({ok, Cfg2}, edm_cfg_server:get([<<"test">>, <<"2">>])),
	%% Don't have a config on it's own so we match the closes in abspath
	?assertEqual({ok, Cfg0}, edm_cfg_server:get([<<"test">>, <<"3">>])),
	?assertEqual({ok, Cfg1}, edm_cfg_server:get([<<"test">>, <<"1">>, <<"a">>])),
	?assertEqual({error, not_found}, edm_cfg_server:get([<<"unknown">>])).
