-module(edm_parser_profile).

-export([
	  parse/2
]).

-define(OP_Map, [{<<"==">>, eq}, {<<">=">>, ge}, {<<"=<">>, le}, {<<"!=">>, ne},
				{<<">">>, gt}, {<<"<">>, lt}, {<<"-">>, eq}]).

parse(Cfg0, ParentRef) ->
	Path = edm_cfg:get(path, Cfg0),
	Profile = edm_env:get(profile, default),
	ProfileTarget = filename:flatten([Profile, ".config"]),

	Files = [filename:join([Path, "edam.config"])
		, filename:join([Path, "build", "edam.config"])
		, filename:join([Path, "build", ProfileTarget])
	],

	Cfg = lists:foldl(fun(File, Acc) ->
		case file:consult(File) of
			{ok, Terms} ->
				edm_log:debug("parser:profile: found ~s", [File]),
				parseterms(ParentRef, Profile, Terms, Acc);
			{error, enoent} ->
				Acc;
			{error, {_, _, _} = Err} ->
				edm_log:error("Parse error in ~s", [File]),
				exit({parser, {consult, Err}})
		end
	end, Cfg0, Files),

	{ok, Cfg}.

parseterms(_ParentRef, _Env, [], Cfg) ->
	Cfg;
parseterms(ParentRef, Env, [{application, App, Terms} | Tail], Cfg) ->
	parseterms(ParentRef, Env, [{application, App, Env, Terms} | Tail], Cfg);
parseterms(ParentRef, EnvA, [{application, _App, EnvB, _Terms} | Tail], Cfg)
	when EnvA =/= EnvB ->
	parseterms(ParentRef, EnvA, Tail, Cfg);
parseterms(ParentRef, Env, [{application, _App, Env, Terms} | Tail], Cfg0) ->
	{ParentRef, Cfg} = lists:foldl(fun stmt/2, {ParentRef, Cfg0}, Terms),
	parseterms(ParentRef, Env, Tail, Cfg).

stmt({catalogs, Cats}, {ParentRef, Cfg}) ->
	N = edm_cat:key(resource),
	StmtCats = lists:keysort(N, lists:foldl(fun expand_cats/2, [], Cats)),
	CfgCats = lists:keysort(N, edm_cfg:get(catalogs, Cfg)),
	NewCats = lists:ukeymerge(N, StmtCats, CfgCats),
	{ParentRef, edm_cfg:set(catalogs, NewCats, Cfg)};
stmt({dependencies, Deps}, Acc) ->
	lists:foldl(fun(Dep, {ParentRef, Cfg0}) ->
		Cfg = edm_cfg:append_dep(expand_dep(Dep), ParentRef, Cfg0),
		{ParentRef, Cfg}
	end, Acc, Deps);
stmt({Key, _}, _Cfg) ->
	exit({parser, {profile, {unknown_stmt, Key}}}).

expand_cats(Resource, Acc) when is_list(Resource) ->
	case edm_cat:new(undefined, list_to_binary(Resource)) of
		{ok, Cat} -> [Cat | Acc];
		false -> Acc
	end;
expand_cats({Resource0, ModPart}, Acc) ->
	Resource = list_to_binary(Resource0),
	Mods = [list_to_atom(filename:flatten([edm_catalog_, ModPart]))],
	case edm_cat:new(undefined, Resource, Mods) of
		{ok, Cat} ->
			[Cat | Acc];
		false ->
			error({parser, {badmodule, Mods}})
	end.

expand_dep(Resource0) when is_list(Resource0) ->
	Resource = list_to_binary(Resource0),
	Ops = [ O || {O, _} <- ?OP_Map],

	case binary:split(Resource, Ops, [trim, global]) of
		[PkgName, Vsn] ->
			Op = find_op(PkgName, Vsn, Resource),
			case lists:keyfind(Op, 1, ?OP_Map) of
				{_, TOp} -> {PkgName, [{version, TOp, Vsn}], []};
				_ -> error({parser, {badop, {Op, Resource}}})
			end;
		[PkgName] ->
			{PkgName, [], []};
		[_|_] = Out ->
			case re:run(lists:last(Out), "^[0-9.-]*$") of
				nomatch ->
					{bjoin(Out, <<$->>), [], []};
				{match, _} ->
					[Vsn0|Name0] = lists:reverse(Out),
					Name1 = bjoin(lists:reverse(Name0), <<$->>),
					{Name1, [{version, eq, Vsn0}], []}
			end
	end;
expand_dep({PkgName0, {Op, Vsn0}, Const, Opts}) ->
	PkgName = atom_to_binary(PkgName0, unicode),
	Vsn = list_to_binary(Vsn0),
	{PkgName, [{version, Op, Vsn} | Const], Opts};
expand_dep({PkgName0, Vsn0, Const, Opts}) ->
	PkgName = atom_to_binary(PkgName0, unicode),
	Vsn = list_to_binary(Vsn0),
	{PkgName, [{version, eq, Vsn} | Const], Opts}.

find_op(PkgName, Vsn, Resource) ->
	{S1, S2, S3} = {size(PkgName), size(Vsn), size(Resource)},
	binary:part(Resource, {S1, S3-S2-S1}).

bjoin(P0, C) ->
	P = [X || X <- P0, <<>> =/= X],
	bjoin2(P, C, <<>>).

bjoin2([S], _C, Acc) ->
	<<Acc/binary, S/binary>>;
bjoin2([S | P], C, Acc) ->
	bjoin2(P, C, <<Acc/binary, S/binary, C/binary>>).
