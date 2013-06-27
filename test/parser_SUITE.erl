-module(parser_SUITE).

-export([
	  all/0
	, groups/0
	, end_per_test/1
	, init_per_test/1
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

init_per_test(Cfg) ->
	{ok, Pid} = edm_cfg_server:start_link(),
	[{cfg_server, Pid} | Cfg].

end_per_test(Cfg) ->
	true = exit(?config(cfg_server), normal),
	Cfg.

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
	edm_env:set(catalogs, [test_cat]),
	DataDir = ?config(data_dir, Cfg),
	in_dir(DataDir, fun() ->
		{ok, Parsed} = edm_parser_profile:parse(edm_cfg:new([{path, DataDir}])),
		Resolved = resolve_cfg(Parsed),
		edm_log:debug("simple: ~p", [Resolved]),
		?assertEqual([], edm_cfg:get(deps, Resolved))
	end).


profile_env(Cfg) ->
	edm_env:set(catalogs, [test_cat]),
	edm_env:set(profile, test),
	DataDir = ?config(data_dir, Cfg),
	in_dir(DataDir, fun() ->
		{ok, Parsed} = edm_parser_profile:parse(edm_cfg:new([{path, DataDir}])),
		[Cats, Deps] = edm_cfg:get([catalogs, deps], Parsed),

		[Cats, Deps]
	end).

in_dir(Dir, Fun) ->
	{ok, Cwd} = file:get_cwd(),
	ok = file:set_cwd(Dir),
	Fun(),
	ok = file:set_cwd(Cwd).

resolve_cfg(Cfg) ->
	iter:foldl(fun({N, C, _} = Dep, Acc) ->
		case edm_cat:resolve(Dep, Acc) of
			false -> Acc;
			Pkg -> resolve_pkg(Pkg, Dep, Acc)
		end
	end, Cfg, Cfg, deps).

resolve_pkg(Pkg, {_,_,_} = Dep, Cfg0) ->
	Deps = lists:delete(Dep, edm_cfg:get(deps, Cfg0)),
	Cfg1 = edm_cfg:set(deps, Deps, Cfg0),
	edm_cfg:set(resolved, [Pkg | edm_cfg:get(resolved, Cfg1)], Cfg1).
