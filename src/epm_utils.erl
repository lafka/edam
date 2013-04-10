-module(epm_utils).

-export([
	  debug/1
	, debug/2
	, info/1
	, info/2
	, err/1
	, err/2
	]).

-export([
	  cmd/1
	, cmd/2
	]).

-export([
	  save_cache/2
	, load_cache/1
	, cache_path/1
	]).

-export([
	  get_domain/1
	]).

-define(format(Type, Msg, Args),
	{_, {H, M, S}} = erlang:universaltime(),
	io:format("[~s] ~b:~b:~b -> " ++ Msg ++ "~n", [Type, H, M, S] ++ Args)).

-define(cache(File), binary_to_list(<<".cache/", File/binary>>)).

info(Msg) ->
	info(Msg, []).

info(Msg, Args) ->
	?format(info, Msg, Args).

debug(Msg) ->
	debug(Msg, []).

debug(Msg, Args) ->
	?format(debug, Msg, Args).

err(Msg) ->
	err(Msg, []).

err(Msg, Args) ->
	?format(error, Msg, Args).

cmd(Cmd) ->
	cmd(Cmd, []).

cmd(Cmd, Args) ->
	Cmd2 = io_lib:format(Cmd, Args),
	debug("+ ~s", [Cmd2]),
	Res = os:cmd(Cmd2),
	debug("> ~s", [Res]),
	Res.

save_cache(Resource, Payload) ->
	File = ?cache(Resource),
	debug("save cache: ~s", [File]),
	Data = term_to_binary(Payload),
	filelib:ensure_dir(File),
	file:write_file(File, Data).

load_cache(Resource) ->
	File = ?cache(Resource),
	debug("lookup cache: ~s", [File]),
	case filelib:is_file(File) of
		true ->
			{ok, Data} = file:read_file(File),
			{ok, binary_to_term(Data)};
		false ->
			false
	end.

cache_path(Resource) ->
	?cache(Resource).

get_domain(Resource) ->
	URL = case binary:split(Resource, <<"://">>) of
		[_, P1] -> P1;
		[P1] -> P1 end,
	[Domain|_] = binary:split(URL, <<$/>>),
	Domain.
