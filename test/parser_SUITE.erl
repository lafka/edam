-module(parser_SUITE).

-export([
	  all/0
	, groups/0
]).

-export([
	  rebar_simple/1
	, otp_simple/1
	, profile_simple/1
	, profile_env/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(testpkg, <<"_test_root">>).

all() ->
	[{group, rebar}, {group, otp}, {group, profile}].

groups() ->
	[ {rebar, [], [rebar_simple]}
	, {otp, [], [otp_simple]}
	, {profile, [], [profile_simple, profile_env]}
	].

rebar_simple(Cfg) ->
	edm_env:set(catalogs, [test_cat]),
	edm_env:set('target.name', ?testpkg),

	DataDir = ?config(data_dir, Cfg),

	{ok, Parsed} = edm_parser_rebar:parse(
		  edm_cfg:new([{path, DataDir}])
		, ?testpkg),

	DepConstraints = [{vsn, regex, "0.1.0"}],
	DepOpts = [
		  {{agent,ref}, "master"}
		, {{agent,remote}, <<"test://github.com/lafka/dep">>}
	],

	Deps = edm_cfg:iter(Parsed, deps),
	?assertMatch([{<<"dep">>, DepConstraints, DepOpts}], Deps),

	Cats = edm_cfg:get(catalogs, Parsed),
	{ok, CatMatch} = edm_cat:new(undefined, <<"test://github.com/lafka/dep">>),
	?assertEqual([CatMatch], [X || X <- Cats, edm_cat:get(module, X) == test_cat]).

otp_simple(_Cfg) ->
	ok.

profile_simple(Cfg) ->
	edm_env:set(catalogs, [edm_catalog_local, test_cat]),
	edm_env:set(include_sys_libs, false),
	edm_env:set('target.name', ?testpkg),

	DataDir = ?config(data_dir, Cfg),

	in_dir(DataDir, fun() ->
		{ok, EdamCfg} = edm_parser_profile:parse(
			  edm_cfg:new([{path, DataDir}])
			, ?testpkg),

		?assertEqual([], edm_cfg:iter(EdamCfg, unresolved)),

		Deps = [edm_pkg:get([name, version], P) || P
			<- edm_cfg:iter(EdamCfg, resolved)],

		?assertMatch(
			[ [<<"dep">>,   <<"0.1.0">>]
			, [<<"dep-b">>, <<"0.2.0">>]
			, [<<"dep-c">>, <<"0.9.1">>]]
		, Deps)

	end).


profile_env(Cfg) ->
	edm_env:set(catalogs, [edm_catalog_local, test_cat]),
	edm_env:set(profile, test),
	edm_env:set(include_sys_libs, false),
	edm_env:set('target.name', ?testpkg),

	DataDir = ?config(data_dir, Cfg),

	in_dir(DataDir, fun() ->
		{ok, EdamCfg} = edm_parser_profile:parse(
			  edm_cfg:new([{path, DataDir}])
			, ?testpkg),

		?assertEqual([], edm_cfg:iter(EdamCfg, unresolved)),

		Deps = [edm_pkg:get([name, version], P) || P
			<- edm_cfg:iter(EdamCfg, resolved)],

		?assertMatch(
			[ [<<"dep">>,   <<"0.1.0">>]
			, [<<"dep-b">>, <<"0.3.0">>]
			, [<<"dep-c">>, <<"1.0.1">>]]
		, Deps)
	end).

in_dir(Dir, Fun) ->
	{ok, Cwd} = file:get_cwd(),
	ok = file:set_cwd(Dir),
	Fun(),
	ok = file:set_cwd(Cwd).
