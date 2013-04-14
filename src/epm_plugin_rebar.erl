-module(epm_plugin_rebar).

-export([
	  parse/2
	]).

-include("epm.hrl").

parse(Path, Pkg) ->
	File = filename:join(Path, "rebar.config"),
	case filelib:is_file(File) of
		true ->
			epm_utils:debug("parsing ~s", [File]),
			{ok, Terms} = file:consult(File),
			parse2(Path, Pkg, Terms);
		false ->
			false
	end.

parse2(_Path, Pkg, Terms) ->
	case lists:keyfind(deps, 1, Terms) of
		{_,Deps} ->
			lists:foldl(fun({Name, Version, Src}, #cfg{repos = R} = Acc) ->
				URL = list_to_binary(erlang:element(2, Src)),
				Backends = [{Backend, Backend:match(URL)} || Backend <- ?backends],
				case [X || {_, true} = X <- Backends] of
					[{Backend, true}|_] ->
						{Alias, Pkgs} = Backend:fetch(undefined, URL),
						Cfg = Acc#cfg{repos = [{Alias, Backend, URL, Pkgs}|R]},
						Dep = parse_dep({Name, Version, Src}, Alias),
						epm_utils:debug("add dep: ~s", [Dep#dep.name]),
						epm_utils:debug("add repo: ~s", [Alias]),
						add_dep(Cfg
							, epm_dep:match_repos(Dep, Cfg)
							, Pkg);
					[] ->
						Acc
				end
			end, #cfg{}, Deps);
		false ->
			false
	end.

parse_dep({Name, ".*", Src}, Repo) ->
	parse_dep({Name, any, Src}, Repo);
parse_dep({Name, Vsn, Src}, Repo) ->
	Ref = case Src of
		{git, _, {branch, Branch}} -> Branch;
		{git, _, {tag, Tag}} -> Tag;
		{git, _, R} -> R;
		_ -> any end,
	#dep{name = atom_to_binary(Name, unicode)
		, version = Vsn
		, ref = Ref
		, repos = [Repo]}.

add_dep(#cfg{deps = Deps, paths = Paths} = Cfg, Dep, Pkg) ->
	Cfg#cfg{
		  paths = [[Dep#dep.name]|Paths]
		, deps = [Dep | Deps]}.
