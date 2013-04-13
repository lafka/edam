-module(epm).

-export([
	  init/0
	, init/1
	, get/1
	, get/2
	, set/2
	]).

-include("epm.hrl").

-define(app, application:get_application()).

-spec init() -> ok.
init() ->
	init([]).

-spec init([{atom(), term()}]) -> ok.
init(Args) ->
	Args2 = lists:ukeymerge(1
		, lists:ukeysort(1, Args)
		, lists:ukeysort(1, [
			  {autofetch, true} % Automatically fetch deps when required
			, {dryrun, false}   % Determines if side effects are allowed
			, {verbosity, info} % What messages to display
			, {basedir, undefined} % The basedir of epm
		])),
	lists:foreach(fun({K,V}) -> set(K, V) end, Args2),
	ok.

-spec get(atom()) -> term().
get(Key) ->
	case get(Key, undefined) of
		undefined ->
			error(badarg, [Key]);
		Val ->
			Val
	end.

-spec get(atom(), Ret) -> Ret when Ret :: term().
get(Key, Default) ->
	case application:get_env(?app, Key) of
		{ok, Val} -> Val;
		undefined -> Default
	end.

-spec set(atom(), Val) -> Val when Val :: term().
set(Key, Val) ->
	ok = application:set_env(?app, Key, Val),
	Val.
