-module(fallback_handler).

-behaviour(cowboy_http_handler).

-export([handlers/1, methods/1, handle_404/2]).

-include_lib("tavern/include/rest_module.hrl").

handlers(_Req) ->
	Handler = handle_404,
	[{'HEAD',   Handler}, {'GET',   Handler},
	 {'POST',   Handler}, {'PATCH', Handler}].

methods(_Req) ->
	['HEAD', 'GET', 'POST', 'PUT', 'PATCH'].

handle_404(Req, State) ->
	{'Not Found', Req, State, [{error, [{message, <<"resource not found">>}, {code, 404]}.

