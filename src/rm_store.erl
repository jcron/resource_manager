-module(rm_store).

-include("rm_store.hrl").

-export([init/0,
             add_conversation/2,
             clone/1,
             conversation_exists/2,
             finalize/0,
             find/1,
             find_all/1,
             initialize_connection/2,
             insert/2,
             remove_conversation/2
            ]).

init() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    mnesia:create_table(?RESOURCE_TABLE_ID, [{attributes, record_info(fields, ?RESOURCE_RECORD)}]),
    mnesia:create_table(?CONVERSATION_TABLE_ID, [{attributes, record_info(fields, ?CONVERSATION_RECORD)}]),
    mnesia:wait_for_tables([?RESOURCE_TABLE_ID, ?CONVERSATION_TABLE_ID], 20000).

add_conversation({Conversation, Connection}, Type) ->
    InsertFun = fun() ->
        ok = mnesia:write(#?CONVERSATION_RECORD{key = {Conversation, Connection}, resource_type = Type})
    end,
    {_, Results} = mnesia:transaction(InsertFun),
    Results.

clone(MasterNode) ->
    ok = mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:add_table_copy(?RESOURCE_RECORD, node(), ram_copies),
    mnesia:add_table_copy(?CONVERSATION_TABLE_ID, node(), ram_copies).

conversation_exists({Conversation, []}, Type) ->
    find_conversation({Conversation, '_'}, Type);
conversation_exists({Conversation, Connection}, Type) ->
    find_conversation({Conversation, Connection}, Type).

finalize() ->
    mnesia:stop().

find(Key) ->
    ReadFun = fun() ->
        mnesia:match_object(?RESOURCE_TABLE_ID, {?RESOURCE_RECORD, Key, '_'}, read)
    end,
    { atomic, [Result | _ ] } = mnesia:transaction(ReadFun),
    Result#?RESOURCE_RECORD.value.

find_all(Key) ->
    ReadFun = fun() ->
        mnesia:match_object(?RESOURCE_TABLE_ID, {?RESOURCE_RECORD, {'_', Key}, '_'}, read)
    end,
    { atomic, Results } = mnesia:transaction(ReadFun),
    [get_connection_from_record(Result) || Result <- Results].

initialize_connection(Connection, Resources) ->
    insert({Connection, total_resources}, Resources),
    insert({Connection, available_resources}, Resources),
    Connection.

insert({Connection, Key}, Value) ->
    InsertFun = fun() ->
        ok = mnesia:write(#?RESOURCE_RECORD{key = {Connection, Key}, value = Value})
    end,
    {atomic, Results} = mnesia:transaction(InsertFun),
    Results.

remove_conversation(Conversation, Connection) ->
    RemoveFun = fun() ->
        ok = mnesia:delete({?CONVERSATION_TABLE_ID, {Conversation, Connection}})
    end,
    {_, Results} = mnesia:transaction(RemoveFun),
    Results.

    
%%% Local functions
find_conversation(Key, Type) ->
    FindFun = fun() ->
        case mnesia:match_object(?CONVERSATION_TABLE_ID, {?CONVERSATION_RECORD, Key, Type}, read) of
            [{?CONVERSATION_RECORD, {_, Connection}, _}] ->
                {true, Connection};
            _ -> {false, []}
        end
    end,
    {_, Results} = mnesia:transaction(FindFun),
    Results.
    
get_connection_from_record(Record) ->
    {Connection, _} = Record#?RESOURCE_RECORD.key,
    Connection.