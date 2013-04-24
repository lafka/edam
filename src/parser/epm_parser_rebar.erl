-module(epm_parser_rebar).

-export([
	  parse/2
	]).

parse(Path, Pkg) ->
	File = filename:join(Path, "rebar.config"),
	case filelib:is_file(File) of
		true ->
			epm:log(debug, "parsing ~s", [File]),
			{ok, Terms} = file:consult(File),
			parse2(Path, Pkg, Terms);
		false ->
			false
	end.

parse2(_Path, Pkg, Terms) ->
	case lists:keyfind(deps, 1, Terms) of
		{_,Deps0} ->
			lists:foreach(fun({_, _, Src} = RawDep) ->
				URL = list_to_binary(erlang:element(2, Src)),

				parse_catalog(URL, Pkg),
				merge_dep(RawDep, Pkg)
			end, Deps0);
		false ->
			false
	end.

parse_catalog(Resource, Pkg) ->
	case epm_catalog:new(undefined, Resource) of
		{ok, Catalog} ->
			epm_store:set(epm_pkg:get(absname, Pkg), catalog, Catalog);
		false ->
			ok
	end.

merge_dep({Name, ".*", Src}, Pkg) ->
	merge_dep({Name, any, Src}, Pkg);
merge_dep({Name, _Vsn, Src}, Pkg) ->
	{Remote, Ref} = case Src of
		{git, Rem, {branch, Branch}} -> {Rem, Branch};
		{git, Rem, {tag, Tag}} -> {Rem, Tag};
		{git, Rem, Ref0} -> {Rem, Ref0};
		{git, Rem} -> {Rem, "master"};
		_ -> error(unsupported_agent, [{Name, _Vsn, Src}, Pkg]) end,

	Version = case Ref of any -> any; Ref -> list_to_bin(Ref) end,
	BinName = atom_to_binary(Name, unicode),

	epm_pkg:new(BinName, [
		  {absname, epm_pkg:get(absname, Pkg) ++ [Name]}
		, {version, Version}
		, {{agent, ref}, Ref}
		, {{agent, remote}, list_to_binary(Remote)}
		]).

list_to_bin(X) when is_list(X) -> list_to_binary(X);
list_to_bin(X) -> X.
