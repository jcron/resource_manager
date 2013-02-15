-define(RESOURCE_TABLE_ID, resource_record).
-define(RESOURCE_RECORD, resource_record).

-define(CONVERSATION_TABLE_ID, conversation_record).
-define(CONVERSATION_RECORD, conversation_record).

-record(?RESOURCE_RECORD, {key, value}).
-record(?CONVERSATION_RECORD, {conversation, resource_type}).