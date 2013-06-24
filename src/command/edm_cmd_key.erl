-module(edm_cmd_key).

-export([
	  main/1
	, description/0
]).

description() ->
	"Management digital signatures".

main(_Args) ->
	io:format("error: key management is not done yet~n").

