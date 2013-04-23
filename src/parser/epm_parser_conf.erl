-module(epm_parser_conf).

-export([
	  parse/2
	]).

parse(Path, Pkg) ->
	case epm_os:epmpp(Path) of
		{ok, Out} ->
			CfgTokens = string:tokens(Out, "\n"),
			parse2(Path, Pkg, CfgTokens);
		{error, _} ->
			false
	end.

parse2(_Path, Pkg, Tokens) ->
	AbsName = epm_pkg:get(absname, Pkg),
	{Size, CfgEnv} = {size(epm:env(env, <<"default">>)), epm:env(env, <<"default">>)},

	lists:foldl(fun
		(<<"catalogs<-">>, _) ->
			epm_store:set(AbsName, catalogs, []),
			catalog;
		(<<"catalogs<<">>, _) -> catalog;
		(<<"catalogs:", Env/binary>>, _) ->
			case Env of
				<<CfgEnv, "<<">> -> catalog;
				_ -> skip
			end;
		(<<"dependencies<-">>, _) ->
			epm_store:set(AbsName, pkgs, []),
			dep;
		(<<"dependencies<<">>, _) -> dep;
		(<<"dependencies:", Env/binary>>, _) ->
			case Env of
				<<CfgEnv:Size/binary, "<<">> -> dep;
				_ -> skip
			end;
		(<<" ", _/binary>>, skip) -> skip;
		(<<" ", Arg/binary>>, catalog) ->
			parse_catalog(Arg, AbsName),
			catalog;
		(<<" ", Arg/binary>>, dep) ->
			parse_dep(Arg, AbsName),
			dep
	end, none, [list_to_binary(X) || X <- Tokens]),

	epm_store:get(AbsName).

parse_catalog(Arg, AbsName) ->
	{Alias, URL} = case binary:split(Arg, <<"<-">>) of
		[A, U] -> {A, U};
		[U] -> {undefined, U} end,

	case epm_catalog:new(Alias, URL) of
		{ok, Catalog} ->
			%% catalogs is pr. config so using paths makes no sense
			epm_store:set(AbsName, catalog, Catalog);
		false ->
			ok
	end,
	ok.

parse_dep(Arg0, AbsName) ->
	{Arg, ExtraOpts} = parse_opts(Arg0),
	[Name|Attrs] = binary:split(Arg, [<<$#>>, <<$@>>, <<$=>>], [global]),

	{_, Opts0} = lists:foldl(fun(Opt, {Pos, Acc}) ->
		NewPos = Pos + size(Opt) + 1,
		case binary:part(Arg, {Pos, 1}) of
			<<$#>> ->
				{NewPos, [{{agent, ref}, Opt} | Acc]};
			<<$@>> ->
				{NewPos, [{catalog, [Opt]} | Acc]};
			<<$=>> ->
				{NewPos, [{version, Opt} | Acc]}
		end
	end, {size(Name), []}, Attrs),

	AbsOpt = {absname, AbsName ++ [Name]},
	Opts = lists:ukeymerge(1
		, [AbsOpt | lists:ukeysort(1, Opts0)]
		, lists:ukeysort(1, ExtraOpts)),

	merge_pkg(AbsName, Name, Opts).

merge_pkg(AbsName, <<$~, Path/binary>>, Opts) ->
	%% @todo 2013-04-23: start PkgPath might overlap with AbsName
	PkgPath = binary:split(Path, <<$/>>, [global]),

	%% If Pkg is found we only need to merge options,
	%% otherwise add a partial pkg that will be appended later
	case epm_store:get(AbsName, {pkg, PkgPath}) of
		false ->
			epm_store:set(AbsName, {partial, PkgPath}, Opts);
		Pkg ->
			epm_store:set(AbsName, {pkg, PkgPath}, epm_pkg:set(Opts, Pkg))
	end;
merge_pkg(AbsName, Name, Opts) ->
	Catalogs = epm_store:get(AbsName, catalogs),
	Pkg = case epm_catalog:select({pkg, Name}, Catalogs) of
		[] ->
			epm_pkg:new(Name, Opts);
		[Ctl|_] ->
			[Pkg0] = epm_catalog:get({pkg, Name}, Ctl),
			lists:foldl(fun({K, V}, Acc) ->
				epm_pkg:set(K, V, Acc) end
			, Pkg0, Opts)
	end,
	epm_store:set(AbsName, {pkg, epm_pkg:get(absname, Pkg)}, Pkg).

parse_opts(Args) ->
	case binary:split(Args, [<<$[>>, <<$]>>], [global]) of
		[Attrs, Opts0, <<>>] ->
			Opts = parse_opts(binary:split(Opts0, <<$,>>, [global]), []),
			{Attrs, Opts};
		[_, _Opts] ->
			error({parser, expectation, [']', void]});
		[Attrs] ->
			{Attrs, []}
	end.

parse_opts([], Acc) ->
	Acc;
parse_opts([H | T], Acc) ->
	[Key0, Val] = binary:split(H, <<$=>>),
	Key = enc_key(Key0),
	parse_opts(T, [enc(Key, Val) | Acc]).

enc_key(Key0) ->
	case [binary_to_atom(P, unicode) || P <- binary:split(Key0, <<$.>>)] of
		[Key] -> Key;
		KeyParts -> list_to_tuple(KeyParts)
	end.

enc(K, <<"\"", Val/binary>>) ->
	{K, binary:part(Val, 0, size(Val) - 1)};
enc(K, <<C/integer, _/binary>> = Val) when C >= $0, C =< $9 ->
	{K, list_to_integer(binary_to_list(Val))};
enc(K, <<C/integer, _/binary>> = Val) when C =:= $_; (C >= $a andalso C =< $z) ->
	{K, binary_to_atom(Val, unicode)};
enc(K, V) ->
	{K, V}.
