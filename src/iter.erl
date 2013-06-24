-module(iter).

-export([
	  map/2
	, map/3
	, foreach/2
	, foreach/3
	, foldl/3
	, foldl/4
	, foldr/3
	, foldr/4
	, flatten/1
	, flatten/2
	, length/1
	, length/2
	, pick/2
	, pick/3
]).

-compile([{no_auto_import, [length/1]}]).

-define(DEFATTR, '_').

-type iter() :: tuple().
-type attr() :: atom(). % Record attribute used for hinting

-spec map(fun((A) -> B), iter()) -> [B] when A :: tuple(), B :: term().
-spec map(fun((A) -> B), iter(), attr()) -> [B] when A :: tuple(), B :: term().

map(Fun, Iter) ->
	map(Fun, Iter, ?DEFATTR).

map(Fun, Iter, Attr) ->
	lists:map(Fun, expand(Iter, Attr)).


-spec foreach(fun((A) -> none()), iter()) -> ok when A :: tuple().
foreach(Fun, Iter) ->
	foreach(Fun, Iter, ?DEFATTR).

foreach(Fun, Iter, Attr) ->
	lists:foreach(Fun, expand(Iter, Attr)).


-spec foldl(fun((A, B) -> B), B, iter()) -> B when A :: tuple(), B :: any().
-spec foldl(fun((A, B) -> B), B, iter(), attr()) -> B when A :: tuple(), B :: any().

foldl(Fun, Acc, Iter) ->
	foldl(Fun, Acc, Iter, ?DEFATTR).

foldl(Fun, Acc, Iter, Attr) ->
	lists:foldl(Fun, Acc, expand(Iter, Attr)).


-spec foldr(fun((A, B) -> B), B, iter()) -> B when A :: tuple(), B :: any().  
-spec foldr(fun((A, B) -> B), B, iter(), attr()) -> B when A :: tuple(), B :: any().  

foldr(Fun, Acc, Iter) ->
	foldr(Fun, Acc, Iter, ?DEFATTR).

foldr(Fun, Acc, Iter, Attr) ->
	lists:foldr(Fun, Acc, expand(Iter, Attr)).


-spec pick([atom()], iter()) -> [any()].
-spec pick([atom()], iter(), attr()) -> [any()].

pick(T, Iter) ->
	pick(T, Iter, ?DEFATTR).

pick(T, Iter, Attr) ->
	map(fun(E) ->
		[Module, _] = expand_tag(E),
		Module:get(T, E)
	end, Iter, Attr).


-spec flatten(iter()) -> list(term()).
-spec flatten(iter(), attr()) -> list(term()).

flatten(Iter) ->
	flatten(Iter, ?DEFATTR).

flatten(Iter, Attr) ->
	lists:flatmap(fun(E) ->
		expand(E, Attr)
	end, expand(Iter, Attr)).


-spec length(iter()) -> non_neg_integer().
-spec length(iter(), attr()) -> non_neg_integer().
length(Iter) ->
	length(Iter, ?DEFATTR).
length(Iter, Attr) ->
	erlang:length(expand(Iter, Attr)).

%% Private
expand(Iter, '_') when is_list(Iter) ->
	Iter;
expand(Iter, Attr) when is_tuple(Iter), size(Iter) > 1 ->
	[Mod, _] = expand_tag(Iter),
	{module, Mod} = code:ensure_loaded(Mod),
	Mod:iter(Iter, Attr).

expand_tag(Rec) when is_tuple(Rec), size(Rec) > 1 ->
	RecName = atom_to_binary(element(1, Rec), unicode),
	Parts = binary:split(RecName, <<$.>>),
	[binary_to_atom(P, unicode) || P <- Parts].
