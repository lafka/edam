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

all() ->
	[{group, rebar}, {group, otp}, {group, profile}].

groups() ->
	[ {rebar, [], [rebar_simple]}
	, {otp, [], [otp_simple]}
	, {profile, [], [profile_simple, profile_env]}
	].

rebar_simple(Cfg) ->
	edm_env:set(catalogs, [test_cat]),
	DataDir = ?config(data_dir, Cfg),
	{ok, Parsed} = edm_parser_rebar:parse(edm_cfg:new([{path, DataDir}])),

	DepConstraints = [{vsn, regex, "0.1.0"}],
	DepOpts = [
		  {{agent,ref}, "master"}
		, {{agent,remote}, <<"test://github.com/lafka/dep">>}
	],

	Deps = edm_cfg:get(deps, Parsed),
	?assertMatch([{<<"dep">>, DepConstraints, DepOpts}], Deps),

	Cats = edm_cfg:get(catalogs, Parsed),
	{ok, CatMatch} = edm_cat:new(undefined, <<"test://github.com/lafka/dep">>),
	?assertEqual([CatMatch], [X || X <- Cats, edm_cat:get(module, X) == test_cat]).

otp_simple(_Cfg) ->
	ok.

profile_simple(Cfg) ->
	edm_env:set(catalogs, [edm_catalog_local, test_cat]),

	DataDir = ?config(data_dir, Cfg),

	in_dir(DataDir, fun() ->
		{ok, Parsed} = edm_parser_profile:parse(edm_cfg:new([{path, DataDir}])),
		EdamCfg = edm_cfg:resolve(Parsed),

		edm_log:debug("parsed: ~p", [EdamCfg]),

		?assertEqual([], edm_cfg:get(deps, EdamCfg)),
		?assertMatch([_,_,_], edm_cfg:get(resolved, EdamCfg)),

		[_A,_B,_C] = lists:keysort(
			  edm_pkg:key(canonical)
			, edm_cfg:get(resolved, EdamCfg))
	end).


profile_env(Cfg) ->
	edm_env:set(catalogs, [edm_catalog_local, test_cat]),
	edm_env:set(profile, test),

	DataDir = ?config(data_dir, Cfg),

	in_dir(DataDir, fun() ->
		{ok, Parsed} = edm_parser_profile:parse(edm_cfg:new([{path, DataDir}])),
		EdamCfg = edm_cfg:resolve(Parsed),

		edm_log:debug("parsed: ~p", [EdamCfg]),

		[Cats, Deps] = edm_cfg:get([catalogs, deps], EdamCfg),
		?assertEqual([], Deps),
		[_A,_B,_C] = edm_cfg:get(resolved, EdamCfg),

		[Cats, [] ]
	end).

in_dir(Dir, Fun) ->
	{ok, Cwd} = file:get_cwd(),
	ok = file:set_cwd(Dir),
	Fun(),
	ok = file:set_cwd(Cwd).
