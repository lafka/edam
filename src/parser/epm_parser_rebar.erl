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
			{[], []}
	end.

parse2(_Path, Pkg, Terms) ->
	case lists:keyfind(deps, 1, Terms) of
		{_,Deps0} ->
			lists:foldl(fun({_, _, Src} = RawDep, {Ctls, Deps}) ->
				URL = list_to_binary(erlang:element(2, Src)),
				case epm_catalog:new(undefined, URL) of
					{ok, Ctl} ->
						Dep = parse_dep(RawDep, [epm_catalog:get(name, Ctl)], Pkg),
						{ [Ctl | Ctls]
						, [Dep | Deps]};
					false ->
						Dep = parse_dep(RawDep, [], Pkg),
						{Ctls, [Dep | Deps]}
				end
			end, {[], []}, Deps0);
		false ->
			false
	end.

parse_dep({Name, ".*", Src}, Ctl, Parent) ->
	parse_dep({Name, any, Src}, Ctl, Parent);
parse_dep({Name, _Vsn, Src}, Ctl, Parent) ->
	{Remote, Ref} = case Src of
		{git, Rem, {branch, Branch}} -> {Rem, Branch};
		{git, Rem, {tag, Tag}} -> {Rem, Tag};
		{git, Rem, Ref0} -> {Rem, Ref0};
		{git, Rem} -> {Rem, "master"};
		_ -> exit(badarg, [Name, _Vsn, Src]) end,

	Version = case Ref of any -> any; Ref -> list_to_bin(Ref) end,
	BinName = atom_to_binary(Name, unicode),

	epm_pkg:new(BinName, [
		  {absname, [BinName | epm_pkg:get(absname, Parent)]}
		, {version, Version}
		, {catalog, Ctl}
		, {{agent, ref}, Ref}
		, {{agent, remote}, list_to_binary(Remote)}
		]).

list_to_bin(X) when is_list(X) -> list_to_binary(X);
list_to_bin(X) -> X.
