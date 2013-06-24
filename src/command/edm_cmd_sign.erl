-module(edm_cmd_sign).

-export([
	  main/1
	, description/0
]).

description() ->
	"Sign application source code".

main(_Args) ->
	io:format("Signing is not done yet~n").

