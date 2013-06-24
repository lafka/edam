-module(parser_SUITE).

-export([
	  all/0
	, groups/0
	, end_per_test/1
	, init_per_test/1
]).

-export([
	  rebar_simple/1
]).

-export([
	  otp_simple/1
]).

-export([
	  profile_simple/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
	[{group, rebar}, {group, otp}, {group, profile}].

groups() ->
	[ {rebar, [], [rebar_simple]}
	, {otp, [], [otp_simple]}
	, {profile, [], [profile_simple]}
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

	DepConstraints = [{vsn, regex, "1.4.0.2"}], 
	DepOpts = [
		  {{agent,ref},"1.4.0.2"}
		, {{agent,remote}, <<"test://github.com/basho/riak_pb">>}
	],
	edm_log:debug("conf: ~p", [Parsed]),
	Deps = edm_cfg:get(deps, Parsed),
	?assertMatch([{<<"riak_pb">>, DepConstraints, DepOpts}], Deps),

	Cats = edm_cfg:get(catalogs, Parsed),
	{ok, CatMatch} = edm_cat:new(undefined, <<"test://github.com/basho/riak_pb">>),
	?assertEqual([CatMatch], [X || X <- Cats, edm_cat:get(module, X) == test_cat]).

otp_simple(_Cfg) ->
	ok.

profile_simple(_Cfg) ->
	ok.
