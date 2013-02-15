-module(rm_store).

-include("rm_store.hrl").

-export([init/0,
             add_conversation/2,
             clone/1,
             conversation_exists/2,
             finalize/0,
             find/1,
             find_all/1,
             initialize_segment/2,
             insert/2,
             remove_conversation/1
            ]).

init() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mnesia:create_table(?RESOURCE_TABLE_ID, [{attributes, record_info(fields, ?RESOURCE_RECORD)}]),
    mnesia:create_table(?CONVERSATION_TABLE_ID, [{attributes, record_info(fields, ?CONVERSATION_RECORD)}]),
    mnesia:wait_for_tables([?RESOURCE_TABLE_ID, ?CONVERSATION_TABLE_ID], 20000).

add_conversation(Conversation, Type) ->
    InsertFun = fun() ->
        ok = mnesia:write(#conversation_record{conversation = Conversation, resource_type = Type})
    end,
    {_, Results} = mnesia:transaction(InsertFun),
    Results.

clone(MasterNode) ->
    ok = mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:add_table_copy(?RESOURCE_RECORD, node(), ram_copies),
    mnesia:add_table_copy(?CONVERSATION_TABLE_ID, node(), ram_copies).

conversation_exists(Conversation, Type) ->
    FindFun = fun() ->
        case mnesia:match_object(?CONVERSATION_TABLE_ID, {?CONVERSATION_RECORD, Conversation, Type}, read) of
            [] -> false;
            _ -> true
        end
    end,
    {_, Results} = mnesia:transaction(FindFun),
    Results.

finalize() ->
    mnesia:stop().

find(Key) ->
    ReadFun = fun() ->
        mnesia:match_object(?RESOURCE_TABLE_ID, {?RESOURCE_RECORD, Key, '_'}, read)
    end,
    { atomic, [Result | _ ] } = mnesia:transaction(ReadFun),
    Result#resource_record.value.

find_all(Key) ->
    ReadFun = fun() ->
        mnesia:match_object(?RESOURCE_TABLE_ID, {?RESOURCE_RECORD, {'_', Key}, '_'}, read)
    end,
    { atomic, Results } = mnesia:transaction(ReadFun),
    [get_segment_from_record(Result) || Result <- Results].

initialize_segment(Segment, Resources) ->
    insert({Segment, total_resources}, Resources),
    insert({Segment, available_resources}, Resources),
    Segment.

insert({Segment, Key}, Value) ->
    InsertFun = fun() ->
        ok = mnesia:write(#resource_record{key = {Segment, Key}, value = Value})
    end,
    {atomic, Results} = mnesia:transaction(InsertFun),
    Results.

remove_conversation(Conversation) ->
    RemoveFun = fun() ->
        ok = mnesia:delete({?CONVERSATION_TABLE_ID, Conversation})        
    end,
    {_, Results} = mnesia:transaction(RemoveFun),
    Results.

    
%%% Local functions
get_segment_from_record(Record) ->
    {Segment, _} = Record#resource_record.key,
    Segment.