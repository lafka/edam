-module(epm_store).

-behaviour(gen_server).

-compile([{no_auto_import, [get/1]}]).

-export([start_link/0]).

%% gen_server callbacks
-export([
	  init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	]).

-export([
	  new/2
	, get/1
	, get/2
	, list/0
	, set/3
	]).

-type absname() :: [binary()].
-type config() :: [{absname(), epm:cfg()}].

-record(state, {
	  configs = [] :: config()
	}).

new(AbsName, Opts) ->
	gen_server:call(?MODULE, {config, {new, AbsName, Opts}}).

get(AbsName) ->
	case catch(gen_server:call(?MODULE, {config, {get, AbsName}})) of
		{'EXIT', Err} -> {error, Err};
		{ok, {_, Cfg}} -> {ok, Cfg}
	end.

get(AbsName0, {pkg, Opt0}) ->
	%% Take special care when mapping pkg names as they m
	get(AbsName0, {pkg, Opt0}, fun reduce_absname/2);
get(AbsName0, Opt0) ->
	get(AbsName0, Opt0, fun(_, Opt) -> Opt end).

get(AbsName0, Opt0, OptFun) ->
	try
		case gen_server:call(?MODULE, {config, {get, AbsName0}}) of
			{ok, {AbsName, Cfg}} ->
				Opt = OptFun(AbsName, Opt0),
				Val = epm:get(Opt, Cfg),
				{ok, Val};
			{error, Err} ->
				{error, Err}
		end
	catch
		C:R ->
			epm:log(error, "store:get: ~p (~p)", [filename:join(AbsName0), {C,R}]),
			epm:log(error, "~p", [erlang:get_stacktrace()]),
			{error, {C, R}}
	end.

list() ->
	gen_server:call(?MODULE, {config, list}).

set(AbsName, Opt, Val) ->
	Tuple = [{Opt, Val}],
	gen_server:call(?MODULE, {config, {set, AbsName, Tuple}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

-spec handle_call(Ev, {reference(), pid()}, #state{}) -> term()
	when Ev :: term().
handle_call({config, list}, _From, State) ->
	{reply, {ok, [ Name || {Name, _} <- State#state.configs]}, State};

handle_call({config, {get, AbsName}}, _From, State) ->
	Reply = find({config, AbsName}, State),
	{reply, Reply, State};

handle_call({config, {new, AbsName, Opts}}, _From, State) ->
	case lists:keymember(root, 1, Opts) of
		true ->
			Cfg = epm:new(Opts),
			case exists({config, AbsName}, State) of
				true ->
					Reply = find({config, AbsName}, State),
					{reply, Reply, State};
				false ->
					{ok, NewState} = save({config, AbsName}, Cfg, State),
					{reply, {ok, Cfg}, NewState}
			end;
		false ->
			{reply, {error, root_not_set}, State}
	end;

handle_call({config, {set, AbsName, Opts}}, _From, State) ->
	case find({config, AbsName}, State) of
		{ok, {CfgAbsName, Cfg0}} ->
			try
				Cfg = lists:foldl(fun({Opt0, V}, Acc) ->
					Opt = reduce_absname(CfgAbsName, Opt0),
					epm:set(Opt, V, Acc)
				end, Cfg0, Opts),

				{ok, NewState} = save({config, CfgAbsName}, Cfg, State),
				{reply, {ok, Cfg}, NewState}
			catch
				Class:Reason  ->
					Trace = erlang:get_stacktrace(),
					epm:log(error, "store:set:  failed to update: ~p"
						, [filename:join(CfgAbsName)]),
					epm:log(error, "~s",
						[format_exception(Class, Reason, Trace, 25)]),
					{reply, {error, {Class, Reason}}, State}
			end;
		Err ->
			{reply, Err, State}
	end;

handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_exception(Class, Reason, Trace, Indent) ->
	Str = lists:flatten(eunit_lib:format_exception({Class, Reason, Trace})),
	Pad = string:chars($ , Indent),
	Lines = lists:map(
		fun(Line) -> [Pad | Line] ++ "\n" end
	, string:tokens(Str, "\n")),
	{TraceStr, [[_|Err]]} = lists:split(length(Lines) - 1, Lines),
	io_lib:format("Caught exception: ~s~n~s",
		[Err, TraceStr]).

%% @private fix overlapping pkg/absname
reduce_absname(AbsName, {pkg, Opt}) ->
	case lists:prefix(AbsName, Opt) of
		true when AbsName =:= Opt ->
			{pkg, [lists:last(Opt)]};
		true ->
			%% last component in AbsName =:= root package in config
			{pkg, [lists:last(AbsName) | lists:subtract(Opt, AbsName)]};
		_ ->
			{pkg, Opt}
	end;
reduce_absname(_AbsName, Opt) ->
	Opt.

%% @private storage api
exists({config, AbsName}, #state{configs = Cfgs}) ->
	lists:keymember(AbsName, 1, Cfgs).

find({config, AbsName}, State) ->
	find({config, AbsName}, AbsName, State).

find({config, AbsName}, Orig, #state{configs = Cfgs} = State) ->
	case lists:keyfind(AbsName, 1, Cfgs) of
		false when length(AbsName) > 0 ->
			NewName = lists:sublist(AbsName, length(AbsName) - 1),
			find({config, NewName}, Orig, State);
		false ->
			{error, no_config};
		{AbsName, Cfg} ->
			{ok, {AbsName, Cfg}}
	end.

%% @private low level storage, does not find parent config
save({config, AbsName}, Cfg, #state{configs = Cfgs} = State) ->
	{ok, State#state{
		configs = lists:keystore(AbsName, 1, Cfgs, {AbsName, Cfg})}}.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

startup_test() ->
	{ok, _P} = start_link(),
	?assertEqual(ok, gen_server:call(?MODULE, stop)).

cfg_test() ->
	Path = filename:absname("./"),
	{ok, _P} = start_link(),
	{ok, Cfg0} = new([<<"test">>], [{root, Path}]),
	{ok, Cfg1} = new([<<"test">>, <<"1">>], [{root, filename:join(Path, <<"1">>)}]),
	{ok, Cfg2} = new([<<"test">>, <<"2">>], [{root, filename:join(Path, <<"2">>)}]),
	?assertEqual({ok, Cfg0}, get([<<"test">>])),
	?assertEqual({ok, Cfg1}, get([<<"test">>, <<"1">>])),
	?assertEqual({ok, Cfg2}, get([<<"test">>, <<"2">>])),
	%% Don't have a config on it's own so we match the closes in abspath
	?assertEqual({ok, Cfg0}, get([<<"test">>, <<"3">>])),
	?assertEqual({ok, Cfg1}, get([<<"test">>, <<"1">>, <<"a">>])),
	?assertEqual({error, not_found}, get([<<"unknown">>])),
	ok = gen_server:call(?MODULE, stop).

epm_compat_test() ->
	Path = filename:absname("./"),
	{ok, _P} = start_link(),
	ExtCfg = epm:new([{root, Path}]),
	{ok, StoredCfg} = new([<<"test">>], [{root, Path}]),
	?assertEqual(ExtCfg, StoredCfg),
	{K, V} = {{pkg, [<<"pkg">>]}, epm_pkg:new(<<"pkg">>)},
	ExtCfg1 = epm:set(K, V, ExtCfg),
	{ok, StoredCfg1} = set([<<"test">>], K, V),
	?assertEqual(ExtCfg1, StoredCfg1),
	ok = gen_server:call(?MODULE, stop).

-endif.
