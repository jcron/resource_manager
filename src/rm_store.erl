-module(rm_store).

-export([init/0, insert/2, find/1, find_all/1]).

-define(TABLE_ID, resources).

init() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

insert(Key, Value) ->
    ets:insert(?TABLE_ID, {Key, Value}).

find(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Value}] -> {ok, Value};
        []             -> {error, not_found}
    end.

find_all(Key) ->
    case ets:match(?TABLE_ID, {{'$1', Key}, '_'}) of
        []      -> {error, not_found};
        Value   -> Value
    end.