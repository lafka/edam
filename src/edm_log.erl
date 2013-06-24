-module(edm_log).

-export([
	  '$handle_undefined_function'/2
	, fmt/1
	, fmt/2
]).

-define(tagsdefault, [ {silent, 0}, {error, 1}, {warning, 2}
                     , {info, 3}, {debug, 4}]).

'$handle_undefined_function'(Fun, Args0) ->
	Level = edm_env:get('log.level', 3),

	try lists:keyfind(Fun, 1, edm_env:get('log.tags', ?tagsdefault)) of
		{Fun, N} when N >= Level ->
			Args1 = case length(Args0) of
				  2 -> Args0
				; 1 -> Args0 ++ [[]] end,
			erlang:apply(fun '_writer'/3, [Fun | Args1]);
		{Fun, _N} ->
			ok;
		false ->
			error(badmatch, {'log.tag', Fun})
	catch C:R ->
		error({handle_undefined_function, {exception, {C, R}}})
	end.

fmt(Str) ->
	fmt(Str, []).

fmt(Str, Args) ->
	io:format(Str ++ "\n", Args).

'_writer'(Tag, Str, Args) ->
	{_, {H, M, S}} = erlang:universaltime(),
	FmtArgs = [H, M, S | Args] ++ [Tag],
	io:format("(~2..0b:~2..0b:~2..0b) " ++ Str ++ " (~s)~n", FmtArgs).

