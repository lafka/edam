-module(edm_cmd_build).
-export([
	  main/1
	, description/0
]).

description() ->
	"Build application artifacts".

main(_Args) ->
	io:format("error: artifact interface is not done yet!~n").

