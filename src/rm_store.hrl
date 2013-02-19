-define(RESOURCE_TABLE_ID, resource_record).
-define(RESOURCE_RECORD, resource_record).

-define(CONVERSATION_TABLE_ID, conversation_record).
-define(CONVERSATION_RECORD, conversation_record).

%%% key = {Connection, Type}
-record(?RESOURCE_RECORD, {key, value}).
%%% key = {Conversation, Connection}
-record(?CONVERSATION_RECORD, {key, resource_type}).