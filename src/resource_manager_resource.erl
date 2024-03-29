%% @author Jeremy Cron <jcron@virtualhold.com>
%% @copyright 2013 VirtualHold.
%% @doc Example webmachine_resource.

-module(resource_manager_resource).

-export([init/1,
             allowed_methods/2,
             content_types_accepted/2,
             content_types_provided/2,
             from_json/2,
             from_url_encoded/2,
             show_resources/2
            ]).

-include_lib("webmachine/include/webmachine.hrl").

-define(GET_METHOD, 'GET').
-define(PUT_METHOD, 'PUT').

-define(JSON_DATA, "application/json").
-define(FORM_DATA, "application/x-www-form-urlencoded").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) -> 
    {[ ?GET_METHOD, ?PUT_METHOD ], ReqData, State }.

content_types_accepted(ReqData, State) ->
    {[{?JSON_DATA, from_json},
        {?FORM_DATA, from_url_encoded}
      ], ReqData, State}.
    
content_types_provided(ReqData, State) ->
    {[{?JSON_DATA, show_resources}], ReqData, State}.

from_json(ReqData, State) ->
    to_json(ReqData, State).

from_url_encoded(ReqData, State) ->
    to_json(ReqData, State).

show_resources(ReqData, State) ->
    show_resources(ReqData, State, get_connection(ReqData, [])).

%%% Local Functions
all_resources(ReqData, State) ->
    Connections = rm_librarian:get_all_connections(),
    ConnectionStruct = get_connections_json(Connections, []),
    encode_to_json(ReqData, State, rm_json:connections(ConnectionStruct)).
    
bad_request(ReqData, State) ->
    json_response(ReqData, State, rm_json:error(bad_request)).

checkin_resource(ReqData, State, Conversation) ->
    {Connection, Total, Available} = rm_librarian:check_in_resource(Conversation),
    ConnectionStruct = rm_json:connection_detail(Connection, Total, Available),
    json_response(ReqData, State, rm_json:connections(ConnectionStruct)).

checkout_resource(ReqData, State, Connection, Conversation) ->
    {Connection, Total, Available} = rm_librarian:check_out_resource(Connection, Conversation),
    ConnectionStruct = rm_json:connection_detail(Connection, Total, Available),
    json_response(ReqData, State, rm_json:connections(ConnectionStruct)).    

encode_to_json(ReqData, State, Json) ->
    {mochijson2:encode(Json), ReqData, State}.

get_content_type_value(ReqData) ->
    mochiweb_headers:get_value('Content-Type', wrq:req_headers(ReqData)).

get_conversation(ReqData, ?JSON_DATA) ->
    get_json_content_value(ReqData, <<"id">>);
get_conversation(ReqData, ?FORM_DATA) ->
    get_url_encoded_value(ReqData, "id").

get_json_content_value(undefined) ->
    [];
get_json_content_value(Value) ->
    binary_to_list(Value).

get_json_content_value(ReqData, Key) ->
    Body = wrq:req_body(ReqData),
    {struct, Json} = mochijson2:decode(Body),
    get_json_content_value(proplists:get_value(Key, Json)).

get_connection(ReqData, ContentType) ->
    get_connection(ReqData, wrq:method(ReqData), ContentType).

get_connection(ReqData, ?GET_METHOD, _) ->
    wrq:get_qs_value("connection", ReqData);
get_connection(ReqData, ?PUT_METHOD, ?JSON_DATA) ->
    get_json_content_value(ReqData, <<"connection">>);
get_connection(ReqData, ?PUT_METHOD, ?FORM_DATA) ->    
    get_url_encoded_value(ReqData, "connection").

get_connections_json([], JsonStruct) ->
    JsonStruct;
get_connections_json([Connection | Connections], JsonStruct) ->
    Total = rm_librarian:get_total_resources(Connection),
    Available = rm_librarian:get_available_resources(Connection),
    get_connections_json(Connections, [rm_json:connection_detail(Connection, Total, Available) | JsonStruct]).

get_url_encoded_value(false) ->
    [];
get_url_encoded_value({_, Value}) ->
    Value.
    
get_url_encoded_value(ReqData, Key) ->
    Body = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    get_url_encoded_value(lists:keyfind(Key, 1, Body)).

json_response(ReqData, State, Response) ->
    ReturnIo = mochijson2:encode(Response),
    ReturnJson = iolist_to_binary(ReturnIo),
    R2 = wrq:append_to_resp_body(ReturnJson, ReqData),
    {true, R2, State}.

no_resource(ReqData, State) ->
    json_response(ReqData, State, rm_json:error(no_resource)).

parse_input_data(ReqData) ->
    ContentType = get_content_type_value(ReqData),
    Connection = get_connection(ReqData, ContentType),
    Conversation = get_conversation(ReqData, ContentType),
    Action = wrq:path_info(action, ReqData),
    {Action, Connection, Conversation}.

show_resources(ReqData, State, undefined) ->
    all_resources(ReqData, State);
show_resources(ReqData, State, Connection) ->
    ConnectionStruct = get_connections_json([Connection], []),
    encode_to_json(ReqData, State, rm_json:connections(ConnectionStruct)).

take_action("checkout", ReqData, State, Connection, Conversation) ->
    checkout_resource(ReqData, State, Connection, Conversation);
take_action("checkin", ReqData, State, _Connection, Conversation) ->
    checkin_resource(ReqData, State, Conversation);
take_action(_Action, ReqData, State, _Connection, _Conversation) ->
    bad_request(ReqData, State).

to_json(ReqData, State) ->
    try
        {Action, Connection, Conversation} = parse_input_data(ReqData),
        take_action(Action, ReqData, State, Connection, Conversation)
    catch
        no_resource -> no_resource(ReqData, State);
        _Error:_Reason -> {false, ReqData, State}
    end.

%%
%% Unit Tests
%%
-ifdef(EUNIT).

allowed_methods_test() -> {[?GET_METHOD, ?PUT_METHOD], reqdata, state} = allowed_methods(reqdata, state).
content_types_provided_test() -> {[{?JSON_DATA, show_resources}], reqdata, state} = content_types_provided(reqdata, state).
content_types_accepted_test() -> {[{?JSON_DATA, from_json}, {?FORM_DATA, from_url_encoded}], reqdata, state} = content_types_accepted(reqdata, state).
    
-endif.