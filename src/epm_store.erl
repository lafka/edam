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
	gen_server:call(?MODULE, {config, {get, AbsName}}).

get(AbsName, Opt) ->
	{ok, Cfg} = gen_server:call(?MODULE, {config, {get, AbsName}}),
	epm:get(Opt, Cfg).

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
		{ok, Cfg0} ->
			Cfg = lists:foldl(fun({K, V}, Acc) ->
				epm:set(K, V, Acc)
			end, Cfg0, Opts),
			{ok, NewState} = save({config, AbsName}, Cfg, State),
			{reply, {ok, Cfg}, NewState};
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


%% @private storage api
exists({config, AbsName}, #state{configs = Cfgs}) ->
	lists:keymember(AbsName, 1, Cfgs).

find({config, AbsName}, State) ->
	find({config, AbsName}, AbsName, State).

find({config, AbsName}, Orig, #state{configs = Cfgs} = State) ->
	case lists:keyfind(AbsName, 1, Cfgs) of
		false when length(AbsName) > 0 ->
			NewName = lists:sublist(AbsName, length(AbsName) - 1),
			find({config, NewName}, State);
		false ->
			Configs = [{configs, [X || {X, _} <- Cfgs]}],
			error(no_config, [{absname, Orig}, Configs]);
		{AbsName, Cfg} ->
			{ok, Cfg}
	end.

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
