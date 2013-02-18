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
-define(JSONDATA, "application/json").
-define(FORMDATA, "application/x-www-form-urlencoded").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) -> 
    {[ 'GET', 'PUT' ], ReqData, State }.

content_types_accepted(ReqData, State) ->
    {[{?JSONDATA, from_json},
        {?FORMDATA, from_url_encoded}
      ], ReqData, State}.
    
content_types_provided(ReqData, State) ->
    {[{?JSONDATA, show_resources}], ReqData, State}.

from_json(ReqData, State) ->
    to_json(ReqData, State).

from_url_encoded(ReqData, State) ->
    to_json(ReqData, State).

show_resources(ReqData, State) ->
    case get_segment(ReqData, []) of
        undefined -> all_resources(ReqData, State);
        Segment ->
            SegmentStruct = get_segments_json([Segment], []),
            encode_to_json(ReqData, State, rm_json:segments(SegmentStruct))
    end.

%%% Local Functions
all_resources(ReqData, State) ->
    Segments = rm_librarian:get_all_segments(),
    SegmentStruct = get_segments_json(Segments, []),
    encode_to_json(ReqData, State, rm_json:segments(SegmentStruct)).
    
bad_request(ReqData, State) ->
    json_response(ReqData, State, rm_json:error(bad_request)).

checkin_resource(ReqData, State, Segment, Conversation) ->
    {Total, Available} = rm_librarian:check_in_resource(Segment, Conversation),
    SegmentStruct = rm_json:segment_detail(Segment, Total, Available),
    json_response(ReqData, State, rm_json:segments(SegmentStruct)).

checkout_resource(ReqData, State, Segment, Conversation) ->
    {Total, Available} = rm_librarian:check_out_resource(Segment, Conversation),
    SegmentStruct = rm_json:segment_detail(Segment, Total, Available),
    json_response(ReqData, State, rm_json:segments(SegmentStruct)).    

encode_to_json(ReqData, State, Json) ->
    {mochijson2:encode(Json), ReqData, State}.

get_content_type_value(ReqData) ->
    mochiweb_headers:get_value('Content-Type', wrq:req_headers(ReqData)).

get_conversation(ReqData, ?JSONDATA) ->
    get_json_content_value(ReqData, <<"id">>);
get_conversation(ReqData, ?FORMDATA) ->
    get_url_encoded_value(ReqData, "id").

get_json_content_value(ReqData, Key) ->
    Body = wrq:req_body(ReqData),
    {struct, Json} = mochijson2:decode(Body),
    case proplists:get_value(Key, Json) of
        undefined -> [];
        Value -> binary_to_list(Value)
    end.

get_segment(ReqData, ContentType) ->
    get_segment(ReqData, wrq:method(ReqData), ContentType).

get_segment(ReqData, 'GET', _) ->
    wrq:get_qs_value("segment", ReqData);
get_segment(ReqData, 'PUT', ?JSONDATA) ->
    get_json_content_value(ReqData, <<"segment">>);
get_segment(ReqData, 'PUT', ?FORMDATA) ->    
    get_url_encoded_value(ReqData, "segment").

get_segments_json([], JsonStruct) ->
    JsonStruct;
get_segments_json([Segment | Segments], JsonStruct) ->
    Total = rm_librarian:get_total_resources(Segment),
    Available = rm_librarian:get_available_resources(Segment),
    get_segments_json(Segments, [rm_json:segment_detail(Segment, Total, Available) | JsonStruct]).

get_url_encoded_value(ReqData, Key) ->
    Body = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    case lists:keyfind(Key, 1, Body) of
        false -> [];
        {Key, Value} -> Value
    end.

json_response(ReqData, State, Response) ->
    ReturnIo = mochijson2:encode(Response),
    ReturnJson = iolist_to_binary(ReturnIo),
    R2 = wrq:append_to_resp_body(ReturnJson, ReqData),
    {true, R2, State}.

no_resource(ReqData, State) ->
    json_response(ReqData, State, rm_json:error(no_resource)).

parse_input_data(ReqData) ->
    ContentType = get_content_type_value(ReqData),
    Segment = get_segment(ReqData, ContentType),
    Conversation = get_conversation(ReqData, ContentType),
    Action = wrq:path_info(action, ReqData),
    {Action, Segment, Conversation}.

to_json(ReqData, State) ->
    try
        {Action, Segment, Conversation} = parse_input_data(ReqData),
        case Action of
            "checkout" -> checkout_resource(ReqData, State, Segment, Conversation);
            "checkin"   -> checkin_resource(ReqData, State, Segment, Conversation);
            _              -> bad_request(ReqData, State)
        end
    catch
        no_resource -> no_resource(ReqData, State);
        _Error:_Reason -> {false, ReqData, State}
    end.

%%
%% Unit Tests
%%
-ifdef(EUNIT).

allowed_methods_test() -> {['GET', 'PUT'], reqdata, state} = allowed_methods(reqdata, state).
content_types_provided_test() -> {[{?JSONDATA, show_resources}], reqdata, state} = content_types_provided(reqdata, state).
content_types_accepted_test() -> {[{?JSONDATA, from_json}, {?FORMDATA, from_url_encoded}], reqdata, state} = content_types_accepted(reqdata, state).
    
-endif.