-module(edm_parser_profile).

-export([
	  parse/1
]).

-define(OP_Map, [{'==', eq}, {'>=', ge}, {'=<', le}, {'!=', ne},
				{'>', gt}, {'<', lt}, {'-', eq}]).

parse(Cfg0) ->
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
				parseterms(Profile, Terms, Acc);
			{error, enoent} ->
				Acc;
			{error, {_, _, _} = Err} ->
				edm_log:error("Parse error in ~s", [File]),
				exit({parser, {consult, Err}})
		end
	end, Cfg0, Files),

	{ok, Cfg}.

parseterms(_Env, [], Cfg) ->
	Cfg;
parseterms(Env, [{application, App, Terms} | Tail], Cfg) ->
	parseterms(Env, [{application, App, Env, Terms} | Tail], Cfg);
parseterms(EnvA, [{application, _App, EnvB, _Terms} | Tail], Cfg)
	when EnvA =/= EnvB ->
	parseterms(EnvA, Tail, Cfg);
parseterms(Env, [{application, _App, Env, Terms} | Tail], Cfg0) ->
	Cfg = lists:foldl(fun stmt/2, Cfg0, Terms),
	parseterms(Env, Tail, Cfg).

stmt({catalogs, Cats}, Cfg) ->
	N = edm_cat:key(resource),
	StmtCats = lists:keysort(N, [expand_cat(C) || C <- Cats]),
	CfgCats = lists:keysort(N, edm_cfg:get(catalogs, Cfg)),
	NewCats = lists:ukeymerge(N, StmtCats, CfgCats),
	edm_cfg:set(catalogs, NewCats, Cfg);
stmt({dependencies, Deps}, Cfg) ->
	NewDeps = edm_cfg:get(deps, Cfg) ++ [expand_dep(D) || D <- Deps],
	edm_cfg:set(deps, NewDeps, Cfg);
stmt({Key, _}, _Cfg) ->
	exit({parser, {profile, {unknown_stmt, Key}}}).

expand_cat(Resource) when is_list(Resource) ->
	edm_log:debug("expand to: ~p", [Resource]),
	{ok, Cat}  = edm_cat:new(undefined, list_to_binary(Resource)),
	Cat;
expand_cat({Resource0, ModPart}) ->
	Resource = list_to_binary(Resource0),
	Mods = [list_to_atom(filename:flatten([edm_catalog_, ModPart]))],
	case edm_cat:new(undefined, Resource, Mods) of
		{ok, Cat} ->
			Cat;
		false ->
			error({parser, {badmodule, Mods}})
	end.

expand_dep(Resource0) when is_list(Resource0) ->
	Resource = list_to_binary(Resource0),
	Ops = [<<"==">>, <<">=">>, <<"=<">>, <<"!=">>, <<$>>>, <<$<>>, <<$->>, <<$ >>],
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
	binary_to_atom(binary:part(Resource, {S1, S3-S2-S1}), unicode).

bjoin(P0, C) ->
	P = [X || X <- P0, <<>> =/= X],
	bjoin2(P, C, <<>>).

bjoin2([S], _C, Acc) ->
	<<Acc/binary, S/binary>>;
bjoin2([S | P], C, Acc) ->
	bjoin2(P, C, <<Acc/binary, S/binary, C/binary>>).
