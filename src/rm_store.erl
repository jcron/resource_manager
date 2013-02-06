-module(rm_store).

-include("rm_store.hrl").

-export([init/0, clone/1, initialize_segment/2, insert/2, find/1, find_all/1, finalize/0]).

init() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mnesia:create_table(?TABLE_ID, [{attributes, record_info(fields, ?RESOURCE_RECORD)}]),
    mnesia:wait_for_tables([?TABLE_ID], 10000).

clone(MasterNode) ->
    ok = mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:add_table_copy(?RESOURCE_RECORD, node(), ram_copies).

initialize_segment(Segment, Resources) ->
    rm_store:insert({Segment, total_resources}, Resources),
    rm_store:insert({Segment, available_resources}, Resources),
    Segment.
    
insert({Segment, Key}, Value) ->
    Insert_fun = fun() ->
        ok = mnesia:write(#resource_record{key = {Segment, Key}, value = Value})
    end,
    mnesia:transaction(Insert_fun).

find(Key) ->
    Read_fun = fun() ->
        mnesia:match_object(?TABLE_ID, {?RESOURCE_RECORD, Key, '_'}, read)
    end,
    { atomic, [Result | _ ] } = mnesia:transaction(Read_fun),
    Result#resource_record.value.

find_all(Key) ->
    Read_fun = fun() ->
        mnesia:match_object(?TABLE_ID, {?RESOURCE_RECORD, {'_', Key}, '_'}, read)
    end,
    { atomic, Results } = mnesia:transaction(Read_fun),
    [[get_segment_from_record(Result)] || Result <- Results]. %we don't really need the extra [] but will require refactoring resource_manager_resource all_resources

finalize() ->
    mnesia:stop().

get_segment_from_record(Record) ->
    {Segment, _} = Record#resource_record.key,
    Segment.