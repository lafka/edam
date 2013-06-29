-module(edm_parser_rebar).

-export([
	  parse/2
]).

parse(Cfg, ParentRef) ->
	Path = edm_cfg:get(path, Cfg),
	File = filename:join(Path, "rebar.config"),

	case file:consult(File) of
		{ok, Terms} ->
			Deps = proplists:get_value(deps, Terms, []),
			NewCfg = lists:foldl(fun({_, _, Src} = Dep, Acc) ->
				URL = list_to_binary(erlang:element(2, Src)),
				append(ParentRef, URL, Dep, Acc)
			end, Cfg, Deps),
			{ok, NewCfg};
		{error, enoent} ->
			false;
		{error, {_, _, _} = Err} ->
			exit({parser, {consult, Err}})
	end.

append(ParentRef, URL, Dep, Cfg0) ->
	Cfg = case edm_cat:new(undefined, URL) of
		{ok, Cat} ->
			edm_cfg:set(catalogs, [Cat | edm_cfg:get(catalogs, Cfg0)], Cfg0);
		false ->
			Cfg0 end,

	edm_cfg:append_dep(dep(Dep), ParentRef, Cfg).

dep({Name, VsnRegex, Src}) ->
	{ok, {Remote, Ref}} = ref(Src),

	BinName = atom_to_binary(Name, unicode),

	Opts = [{{agent, ref}, Ref}, {{agent, remote}, list_to_binary(Remote)}],
	{BinName, [{vsn, regex, VsnRegex}], Opts}.


ref({git, Rem, {branch, Branch}}) -> {ok, {Rem, Branch}};
ref({git, Rem, {tag, Tag}}) -> {ok, {Rem, Tag}};
ref({git, Rem, Ref0}) -> {ok, {Rem, Ref0}};
ref({git, Rem}) -> {ok, {Rem, "master"}};
ref(AgentOpts) -> {error, {?MODULE, agent, AgentOpts}}.
