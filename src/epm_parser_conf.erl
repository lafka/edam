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
	{_, Acc} = lists:foldl(fun
		(<<"catalogs<-">>, {_, {_, Deps}}) ->
			{catalog, {[], Deps}};
		(<<"catalogs<<">>, {_, Acc}) ->
			{catalog, Acc};
		(<<"dependencies<-">>, {_, {Catalogs, _}}) ->
			{dep, {Catalogs, []}};
		(<<"dependencies<<">>, {_, Acc}) ->
			{dep, Acc};
		(<<" ", Arg/binary>>, {catalog, {Catalogs, Deps}}) ->
			{catalog, match_catalog(Arg, Catalogs, Deps)};
		(<<" ", Arg/binary>>, {dep, {Catalogs, Deps}}) ->
			{dep, match_dep(Arg, Catalogs, Deps)}
	end, {none, {[], []}}, [list_to_binary(X) || X <- Tokens]),
	Acc.

match_catalog(Arg, Catalogs, Deps) ->
	{Alias, URL} = case binary:split(Arg, <<"<-">>) of
		[A, U] -> {A, U};
		[U] -> {undefined, U} end,
	case epm_catalog:new(Alias, URL) of
		{ok, Catalog} ->
			{[Catalog | Catalogs], Deps};
		false ->
			{Catalogs, Deps}
	end.

match_dep(Arg0, Catalogs, Deps) ->
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
	{Catalogs, [new_pkg(Name, Opts, Catalogs) | Deps]}.

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
	[Key, Val] = binary:split(H, <<$=>>),
	parse_opts(T, [{enc_key(Key), enc_val(Val)} | Acc]).

enc_key(Key0) ->
	case [binary_to_atom(P, unicode) || P <- binary:split(Key0, <<$.>>)] of
		[Key] -> Key;
		KeyParts -> list_to_tuple(KeyParts)
	end.

enc_val(<<"\"", Val/binary>>) ->
	binary:part(Val, 0, size(Val) - 1);
enc_val(Val0) ->
	Val0.
