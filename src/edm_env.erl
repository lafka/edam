-module(edm_env).

-export([
	  get/1
	, get/2
	, set/2
]).

-define(app, edm).

-spec get(atom()) -> term().
get(Key) ->
	case application:get_env(?app, Key) of
		{ok, Val} ->
			Val;
		undefined ->
			error(badmatch, [Key])
	end.

-spec get(atom(), Default :: term()) -> term().
get(Key, Default) ->
	case application:get_env(?app, Key) of
		{ok, Val} ->
			Val;
		undefined ->
			Default
	end.

-spec set(atom(), Val) -> Val when Val :: term().
set(Key, Val) ->
	edm_log:debug("setting '~s' to ~p", [Key, Val]),
	application:set_env(?app, Key, Val),
	Val.
