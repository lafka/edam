-module(edm_catalog_local).

-export([
	  name/0
	, match/1
	, fetch/1
	, resource/2
	]).

resource(Pkg, Ctl) ->
	[Name, Vsn] = edm_pkg:get(name, version, Pkg),
	Resource = edm_pkg:get(resource, Ctl),

	Wildcard = case Vsn of
		any ->
			filename:join(Resource, <<Name/binary, "{,-*}">>);
		Vsn ->
			WC = <<Name/binary, "{,-", Vsn/binary, "}">>,
			filename:join(Resource, WC) end,

	case filelib:wildcard(Wildcard) of
		[Dep] ->
			{ok, Dep};
		[] ->
			Str = io_lib:format("could not find resource ~p in catalog ~p"
				, [{Name, Vsn}, edm_cat:get(name, Ctl)]),
			{error, Str};
		Deps ->
			Str = io_lib:format("multiple choices: ~p", [Deps]),
			{error, Str}
	end.

name() ->
	<<"local">>.

match(<<"/", _/binary>>) -> true;
match(<<_, ":\\", _/binary>>) -> true;
match(<<"file://", _/binary>>) -> true;
match(_) -> false.

fetch(Ctl) ->
	Resource = edm_cat:get(resource, Ctl),
	{ok, Files} = file:list_dir(Resource),

	Pkgs = lists:foldl(fun(File, Acc) ->

		[Name, Vsn] = reduce_name(File),

		Path = filename:join(Resource, File),
		Pkg = edm_pkg:new(Name, [template, {version, Vsn}
			, {agent, edm_agent_fs}, {{agent, source}, Path}]),

		[Pkg | Acc]
	end, [], Files),

	Ctl1 = edm_cat:set(pkgs, Pkgs, Ctl),
	{ok, edm_cat:set(pkgs, Pkgs, Ctl1)}.

% @todo olav 2013-06-26: This will now work pkgs w/patch level or file extensions
% rewrite to use regex
reduce_name(File) ->
	case string:tokens(File, "-") of
		[Name0] ->
			[list_to_binary(Name0), any];
		[_|_] = Out ->
			case re:run(lists:last(Out), "^[0-9.-]*$") of
				nomatch ->
					[lists:flatten(Out), any];
				{match, _} ->
					[Vsn0|Name0] = lists:reverse(Out),
					Name1 = lists:flatten(lists:reverse(Name0)),
					[list_to_binary(X) || X <- [Name1, Vsn0]]
			end
	end.
