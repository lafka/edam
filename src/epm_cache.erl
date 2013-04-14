-module(epm_cache).

-export([
	  path/1
	, stale/1
	, load/1
	, save/2
	]).

-spec path([Comp]) -> Comp when Comp :: file:filename_all().
path(Path) ->
	filename:join([epm:env(cachedir) | Path]).

-spec stale(file:filename_all()) -> boolean().
stale(File) ->
	0 =:= filelib:last_modified(File).

load(File) ->
	case {0 =/= filelib:last_modified(File), stale(File)} of
		{_, true} ->
			{error, expired};
		{false, _} ->
			{error, noent};
		{true, false} ->
			{ok, Raw} = file:read_file(File),
			{ok, binary_to_term(Raw)}
	end.

save(File, Data) ->
	filelib:ensure_dir(File),
	ok = file:write_file(File, term_to_binary(Data)).
