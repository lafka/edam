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

parse2(_Path, _Pkg, Tokens) ->
	{Size, CfgEnv} = {size(epm:env(env, <<"default">>)), epm:env(env, <<"default">>)},
	{_, {Cats, Deps, Cb}} = lists:foldl(fun
		(<<"catalogs<-">>, {_, {_, Deps, Cb}}) ->
			{catalog, {[], Deps, Cb}};
		(<<"catalogs<<">>, {_, Acc}) ->
			{catalog, Acc};
		(<<"catalogs:", Env/binary>>, {_, Acc}) ->
			case Env of
				<<CfgEnv, "<<">> -> {catalog, Acc};
				_ -> {skip, Acc}
			end;
		(<<"dependencies<-">>, {_, {Catalogs, _, _Cb}}) ->
			{dep, {Catalogs, [], []}};
		(<<"dependencies<<">>, {_, Acc}) ->
			{dep, Acc};
		(<<"dependencies:", Env/binary>>, {_, Acc}) ->
			case Env of
				<<CfgEnv:Size/binary, "<<">> ->
					{dep, Acc};
				_ ->
					{skip, Acc}
			end;
		(<<" ", _/binary>>, {skip, Acc}) ->
			{skip, Acc};
		(<<" ", Arg/binary>>, {catalog, {Catalogs, Deps, Callbacks}}) ->
			{catalog, match_catalog(Arg, Catalogs, Deps, Callbacks)};
		(<<" ", Arg/binary>>, {dep, {Catalogs, Deps, Callbacks}}) ->
			{dep, match_dep(Arg, Catalogs, Deps, Callbacks)}
	end, {none, {[], [], []}}, [list_to_binary(X) || X <- Tokens]),
	{Cats, lists:reverse(Deps), Cb}.

match_catalog(Arg, Catalogs, Deps, Callbacks) ->
	{Alias, URL} = case binary:split(Arg, <<"<-">>) of
		[A, U] -> {A, U};
		[U] -> {undefined, U} end,
	case epm_catalog:new(Alias, URL) of
		{ok, Catalog} ->
			{[Catalog | Catalogs], Deps, Callbacks};
		false ->
			{Catalogs, Deps, Callbacks}
	end.

match_dep(Arg0, Catalogs, Deps, Callbacks) ->
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
	Opts = lists:ukeymerge(1
		, lists:ukeysort(1, Opts0)
		, lists:ukeysort(1, ExtraOpts)),
	case Name of
		<<$~, Path/binary>> ->
			PatchFun = fun(Cfg) ->
				AbsPath = binary:split(Path, <<"/">>, [global]),
				Pkg0 = epm:get({dep, AbsPath}, Cfg),

				Pkg = lists:foldl(fun({K, V}, Acc) ->
					epm_pkg:set(K, V, Acc)
				end, Pkg0, Opts),

				epm:set({dep, AbsPath}, Pkg, Cfg)
			end,
			{Catalogs, Deps, [PatchFun | Callbacks]};
		_ ->
			{Catalogs, [new_pkg(Name, Opts, Catalogs) | Deps], Callbacks}
	end.

%% @private construct a new pkg, possibly using base from pkg found in
%% catalogs. This does only check with currently known configuration
new_pkg(Name, Opts, Catalogs) ->
	case epm_catalog:select({pkg, Name}, Catalogs) of
		[] ->
			epm_pkg:new(Name, Opts);
		[Ctl|_] ->
			[Pkg] = epm_catalog:get({pkg, Name}, Ctl),
			lists:foldl(fun({K, V}, Acc) ->
				epm_pkg:set(K, V, Acc) end
			, Pkg, Opts)
	end.

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
