-module(edm_main).

-export([
	  main/1
	, script/0
	]).

-record('edm_main.cfg', {
	  catalogs = [] :: [edm_cat:cat()]
	, pkgs = [] :: [edm_pkg:pkg()]
	, opts = [] :: [{atom(), term()}]
	, root :: binary()
	, isolate = false :: boolean()
	}).

-opaque cfg() :: #'edm_main.cfg'{}.

-export_type([cfg/0]).

-define(CMD, "edm_cmd_").
-define(CMDATOM(C), list_to_atom(?CMD ++ C)).

script() ->
	filename:basename(escript:script_name()).

main(Args0) ->
	{ok, _} = edm_cfg_server:start_link(),

	Name = escript:script_name(),
	{ok, Paths} = erl_prim_loader:get_path(),
	CodePath = filename:join([Name, Name, ebin]),
	ok = erl_prim_loader:set_path([CodePath | Paths]),

	set_default_env(),

	case parse_args(Args0) of
		[] ->
			help();
		Args ->
			{ok, Cfgs} = edm_cfg:parse(edm_env:get('target.path'), [save]),

			ExpandCfg = fun(Path) ->
				edm_log:debug("main: found configuration ~p", [Path]),
				{ok, Cfg} = edm_cfg_server:get(Path),
				Cfg
			end,

			main2(Args, lists:map(ExpandCfg, Cfgs))
	end.

set_default_env() ->
	{ok, Cwd} = file:get_cwd(),

	edm_env:set('target.path', Cwd),
	edm_env:set('parsers', [
		  edm_parser_rebar
		, edm_parser_otp
		, edm_parser_profile
	]),
	edm_env:set('catalogs', [
		  edm_catalog_local
		, edm_catalog_mem
%		, edm_catalog_github
%		, edm_catalog_sshremote
	]),
	ok.

main2(["help"], _Cfgs) ->
	help();

main2([Command | Args], Cfgs) ->
	try
		command(Command, Args, Cfgs)
    catch
		exit:{edm, Err} ->
			io:format(Err);
		Class:Reason ->
			io:format("~nCaught exception: ~p:~n~69..=s~n~s", [{Class, Reason}, "="
				, eunit_lib:format_exception({Class,Reason,erlang:get_stacktrace()})])
    end.

parse_args(Args) ->
	parse_args(Args, []).

parse_args([], Acc) ->
	lists:reverse(Acc);

parse_args([Arg | T], Acc) ->
	parse_args(T, [Arg | Acc]).

command(Cmd, Args, Cfgs) ->
	Module = ?CMDATOM(Cmd),

	case code:ensure_loaded(Module) of
		{module, Module} ->
			Module:main(Args, Cfgs);
		{error, nofile} ->
			io:format("~s~n~nNo such command '~s'~n", [usage(), Cmd])
	end.

commands() ->
	{ok, Paths} = erl_prim_loader:get_path(),

	lists:foldl(fun(Path, Commands) ->
		{ok, Beams} = erl_prim_loader:list_dir(Path),
		[ {filename:basename(File, ".beam"), extract_cmd(File)} || File
			<- Beams
			, filename:extension(File) == ".beam"
			, lists:prefix(?CMD, File)]
		++ Commands
	end, [], Paths).

extract_cmd(?CMD ++ Cmd) ->
	filename:basename(Cmd, ".beam").

usage() ->
	Commands = lists:foldr(fun
		({_Mod, Cmd}, [])  -> [Cmd];
		({_Mod, Cmd}, Str) -> [Cmd ++ "|" | Str]
	end,  [], commands()),

	io_lib:format("usage: ~s <~s>", [script(), Commands]).

help() ->
	Padding = lists:foldl(fun({_, C}, Acc) ->
		max(length(C), Acc)
	end, 4, commands()), % 4 =:= length("help")

	Fmt = "  ~-" ++ integer_to_list(Padding) ++ "s  -- ~s~n",
	HelpText = io_lib:format(Fmt, [help, "Show this help text"]),

	Cmds = [ HelpText | lists:foldr(fun({Mod0, Cmd}, Acc) ->
		Mod = list_to_atom(Mod0),
		[io_lib:format(Fmt, [Cmd, Mod:description()]) | Acc]
	end, [], commands())],

	io:format("~s~n~nCommands:~n~s~n" , [usage(), Cmds]).
