-module(epm_os).

-export([
	  epmpp/1
	, cmd/1
	, cmd/2
	, cmd/3
	]).

-export([
	  mktemp/0
	, mktemp/1
	, mktemp/2
	, temp_name/1
	, temp_name/2
	]).

-spec epmpp([file:filename_all()]) -> {ok, Out} | {error, {integer(), Out}}
	when Out :: string().
epmpp(Path) when is_binary(Path) ->
	epmpp(binary_to_list(Path));
epmpp(Path) ->
	cmd(filename:join(epm:env(basedir), "bin/epmpp") ++ " " ++ Path).

-spec cmd(iolist()) -> {ok, Out} | {error, {integer(), Out}}
	when Out :: string().
cmd(Cmd) ->
	cmd(Cmd, []).

-spec cmd(iolist(), [term()]) -> {ok, Out} | {error, {integer(), Out}}
	when Out :: string().
cmd(Cmd, Args) ->
	{ok, Cwd} = file:get_cwd(),
	cmd(Cmd, Args, Cwd).

-spec cmd(iolist(), [term()], string()) -> {ok, Out} | {error, {integer(), Out}}
	when Out :: string().
cmd(Cmd, Args, Path) ->
	Closure = case file:get_cwd() of
		{ok, Path} ->
			fun() -> ok end;
		{ok, OldCwd} ->
			ok = file:set_cwd(Path),
			fun() -> ok = file:set_cwd(OldCwd) end end,

	File = mktemp("epm-eval"),

	Cmd2 = io_lib:format(Cmd, Args),
	Cmd3 = io_lib:format("~s > ~s 2>&1; echo $?", [Cmd2, File]),

	epm:log(debug, "+ ~s $ ~s", [Path, Cmd2]),

	{Status, "\n"} = string:to_integer(os:cmd(Cmd3)),
	{ok, Output0} = file:read_file(File),
	Output = binary_to_list(Output0),
	Closure(),
	ok = file:delete(File),

	case Status of
		0 -> {ok, Output};
		N -> {error, {N, Output}}
	end.

mktemp() ->
	mktemp(mktemp).
mktemp(Prefix) ->
	mktemp(Prefix, "/tmp").
mktemp(Prefix, Dir) ->
	File     = temp_name(Dir, Prefix),
	filelib:ensure_dir(File),
	{ok, FD} = file:open(File, [write, exclusive]),
	ok       = file:close(FD),
	File.

temp_name(Dir, "") -> temp_name(Dir ++ "/");
temp_name(Dir, Prefix) -> temp_name(filename:join(Dir, Prefix)).
temp_name(Stem) ->
	filename:join(Stem
		, integer_to_list(crypto:rand_uniform(0, 1 bsl 127))).
