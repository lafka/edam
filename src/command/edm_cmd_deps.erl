-module(edm_cmd_deps).

-export([
	  main/2
	, name/0
	, description/0
	, search/2
	, fetch/2
	, sync/2
	, check/2
	, show/2
]).

name() ->
	deps.

description() ->
	"Interact with package dependencies".

main([], Cfgs) ->
	help(Cfgs);
main(["help"], Cfgs) ->
	help(Cfgs);

main([Cmd0 | Args], Cfgs) ->
	Cmd = list_to_atom(Cmd0),

	try ?MODULE:Cmd(Args, Cfgs)
	catch error:function_clause ->
		case erlang:function_exported(?MODULE, Cmd, 2) of
			true ->
				io:format("error: invalid arguments for '~s ~s ~s'~n~n"
					, [edm_main:script(), name(), Cmd]),

				Trace = eunit_lib:format_exception({error, function_exported, erlang:get_stacktrace()}),
				io:format(Trace);
			false ->
				io:format("~s~n~nerror: no such command '~s ~s ~s'~n"
					, [usage(), edm_main:script(), name(), Cmd])
		end
	end.

help(_Cfgs) ->
	Targets = io_lib:format(
		"search <pkg>~n"
		"    Search all catalogs for package.~n"
		"fetch [<pkg>,..]~n"
		"    Fetch one more or more package to local store for later use.~n"
		"sync [<pkg>,..]~n"
		"    Update all dependencies, optionally only update <pkgs> if specfied.~n"
		"check [<pkg>,..]~n"
		"    Check if dependencies are consistent.~n"
		"show [--attrs <attr,..>] [<pkg>,..]~n"
		"    Print out current status either for all packages or those defined in <pkg> list.~n", []),

	io:format("~s~n~n~s~n", [usage(), Targets]).

targets() ->
	["help", "search", "fetch", "sync", "check", "show"].

usage() ->
	Targets = lists:foldr(fun
		(Cmd, [])  -> [Cmd];
		(Cmd, Str) -> [Cmd ++ "|" | Str]
	end,  [], targets()),

	io_lib:format("usage: ~s ~s <~s>", [edm_main:script(), name(), Targets]).

search([_], _Cfgs) ->
	io:format("error: deps search is not implemented~n").

fetch([], Cfgs) ->
	edm_log:debug("fetching all dependencies"),
	Pkgs = [{Cfg, iter:flatten(Cfg)} || Cfg <- Cfgs],
	edm_log:debug("fetching: ~p", [Pkgs]),
	fetch2(Pkgs);

fetch(Pkgs0, Cfgs) ->
	edm_log:debug("fetching dependencies: ~p", [Pkgs0]),
	Pkgs1 = [list_to_binary(X) || X <- Pkgs0],

	Filter = fun(Cfg) ->
		[P || P <- edm_pkg:flatten(Cfg)
			, lists:member(edm_pkg:get(name, P), Pkgs1)]
	end,

	Pkgs = lists:foldl(fun(Cfg, Acc) ->
		[{edm_env:get(root, Cfg), Filter(Pkgs0, Cfg)} | Acc]
	end, [], Cfgs),

	fetch2(Pkgs).

fetch2([]) ->
	ok;

fetch2([{Cfg, Pkgs} | Tail]) ->
	edm_log:info("fetching ~b pkgs from ~s"
		, [length(Pkgs), edm_env:get(root, Cfg)]),

	iter:foreach(fun(Pkg) -> edm_pkg:fetch(Pkg, Cfg) end, Pkgs),
	fetch2(Tail).

sync([], Cfgs) ->
	Pkgs = [{Cfg, iter:flatten(Cfg)} || Cfg <- Cfgs],
	sync2(Pkgs);

sync(_Pkgs, _Cfgs) ->
	%% todo match config w/pkg
	sync2([]).

sync2(Pkgs0) ->
	lists:foreach(fun({Cfg, Pkgs}) ->
		iter:foldl(fun(P, Acc) ->
			Key = edm_pkg:get([name,version,agent], P),
			case lists:keymember(Key, 1, Acc) of
				true ->
					ok;
				false ->
					edm_pkg:sync(P, Cfg),
					[Key | Acc]
			end
		end, [], Pkgs)
	end, Pkgs0).

check(_Args, _Cfgs) ->
	io:format("check check check~n").

%% @todo olav 2013-06-17; take list of args corresponding to pkgs names (name or absname)
%% @todo olav 2013-06-17; make visualization for showing dependency tree
show(Args0, Cfgs) ->
	Args = show_opts(Args0),
	Attrs = edm_env:get('print.format', [absname, version, synced]),

	show2(Args, Attrs, Cfgs).

show_opts(Opts) ->
	show_opts(Opts, []).

show_opts([], Acc) ->
	lists:reverse(Acc);
show_opts(["--attrs",  Attrs0 | Tail], Acc) ->
	Attrs = string:tokens(Attrs0, ","),
	try
		edm_env:set('print.format', [list_to_existing_atom(X) || X <- Attrs])
	catch error:badarg ->
		edm_log:error("could not use format: ~p", [Attrs]),
		edm_log:error("~p", [erlang:get_stacktrace()])
	end,
	show_opts(Tail, Acc);
show_opts(["-" ++ _ = Arg | _], _) ->
	exit({edm, io_lib:format("unknown option: ~p in deps.print\n", [Arg])});
show_opts([Arg | Tail], Acc) ->
	show_opts(Tail, [Arg | Acc]).

show2(_Args, Attrs, Cfgs) ->
	lists:foreach(fun(Cfg) ->
		edm_log:fmt(":: Dependencies in config ~s", edm_cfg:get([path], Cfg)),

		L = length(Attrs),
		{_, Fmt0} = lists:foldl(fun(_Key, {N, Acc}) ->
			{N + 1, case Acc of
				[] -> "(:~s ~~p";
				Acc when N == L -> Acc ++ ", :~s ~~p)\n";
				Acc -> Acc ++ ", :~s ~~p" end}
		end, {1, []}, Attrs),

		Fmt = lists:flatten(io_lib:format(Fmt0, Attrs)),

		print_loop(Cfg, Attrs, Fmt)
	end, Cfgs).

print_loop(Cfg, Attrs, Fmt) ->
	case iter:length(Cfg) of
		0 ->
			edm_log:fmt("No dependencies in configuration ~p", edm_cfg:get([path], Cfg));
		_ ->
			iter:foreach(fun(P) ->
				epm_log:fmt(Fmt, edm_pkg:get(Attrs, P)),
				print_loop(P, Attrs, Fmt)
			end, Cfg)
	end.
