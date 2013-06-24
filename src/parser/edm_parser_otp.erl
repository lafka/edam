%% OTP .app file parser
%%
%% Parses an app file, appends a static catalog for each app with all
%% included packages. Each required application in the app file will
%% be added as a dependency.

-module(edm_parser_otp).

-export([
	  parse/1
]).

%% @todo olav 2013-06-24; Write tests for apps/* style projects,
parse(Cfg) ->
	Path = edm_cfg:get(path, Cfg),
	Wildcard = filename:join([Path, "{ebin,apps}", "**.app"]),

	Files = filelib:wildcard(Wildcard),
	edm_log:debug("parser:otp: wildcard '~s' found ~b files"
		, [Wildcard, length(Files)]),

	NewCfg = lists:foldl(fun(File, Acc) ->
		edm_log:debug("parser:otp: parsing file ~s", [File]),


		{PkgConst, Cat} = parsefile(File),
		Apps = edm_cfg:get(deps, Acc) ++ PkgConst,
		Cats = edm_cfg:get(catalogs, Acc) ++ [Cat],

		edm_cfg:set(catalogs, Cats,
			  edm_cfg:set(deps, Apps, Acc))
	end, Cfg, Files),

	{ok, NewCfg}.

parsefile(File) ->
	{ok, [{application, Name, Terms}]} = file:consult(File),

	BinName = atom_to_binary(Name, unicode),
	Version = proplists:get_value(vsn, Terms, any),

	Opts = [
		  {version, list_to_binary(Version)}
		, {deps, []}
		, {path, approot(File)}
		, {template, true}
	],

	Pkg = edm_pkg:new(BinName, Opts),
	{ok, Ctl0} = edm_cat:new(approot(File), mem),
	Ctl = edm_cat:set(pkgs, [Pkg], Ctl0),
	Deps0 = [{atom_to_binary(X, unicode), [], []} || X
		<- proplists:get_value(applications, Terms, [])],
	Deps = [{BinName, [{version, eq, Version}], []} | Deps0],

	{Deps, Ctl}.

approot(File) ->
	filename:dirname(filename:dirname(filename:absname(File))).
