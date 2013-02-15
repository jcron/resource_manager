%% @author Jeremy Cron <jcron@virtualhold.com>
%% @copyright 2013 VirtualHold.
%% @doc Example webmachine_resource.

-module(resource_manager_resource).

-export([init/1,
             allowed_methods/2,
             content_types_accepted/2,
             content_types_provided/2,
             from_json/2,
             to_json/2
            ]).

-include_lib("webmachine/include/webmachine.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) -> 
    {[ 'GET', 'PUT' ], ReqData, State }.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.
    
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

from_json(ReqData, State) ->
    Action = wrq:path_info(action, ReqData),
    try
        Segment = get_segment(ReqData),
        case Action of
            "checkout" -> checkout_resource(ReqData, State, Segment);
            "checkin"  -> checkin_resource(ReqData, State, Segment);
            _          -> bad_request(ReqData, State)
        end
    catch
        no_resource -> json_response(ReqData, State, rm_json:error(no_resource));
        _Error:_Reason -> {false, ReqData, State}
    end.
    
to_json(ReqData, State) ->
    show_resources(ReqData, State).

%%% Local Functions
all_resources(ReqData, State) ->
    Segments = rm_librarian:get_all_segments(),
    SegmentStruct = get_segments_json(Segments, []),
    to_json(ReqData, State, rm_json:segments(SegmentStruct)).
    
bad_request(ReqData, State) ->
    json_response(ReqData, State, rm_json:error(bad_request)).

checkin_resource(ReqData, State, Segment) ->
    {Total, Available} = rm_librarian:check_in_resource(Segment),
    SegmentStruct = rm_json:segment_detail(Segment, Total, Available),
    json_response(ReqData, State, rm_json:segments(SegmentStruct)).

checkout_resource(ReqData, State, Segment) ->
    {Total, Available} = rm_librarian:check_out_resource(Segment),
    SegmentStruct = rm_json:segment_detail(Segment, Total, Available),
    json_response(ReqData, State, rm_json:segments(SegmentStruct)).    

get_segment(ReqData) ->
    get_segment(ReqData, wrq:method(ReqData)).
    
get_segment(ReqData, 'GET') ->
    wrq:get_qs_value("segment", ReqData);
get_segment(ReqData, 'PUT') ->
    Body = wrq:req_body(ReqData),
    {struct, Json} = mochijson2:decode(Body),
    binary_to_list(proplists:get_value(<<"segment">>, Json)).

get_segments_json([], JsonStruct) ->
    JsonStruct;
get_segments_json([Segment | Segments], JsonStruct) ->
    Total = rm_librarian:get_total_resources(Segment),
    Available = rm_librarian:get_available_resources(Segment),
    get_segments_json(Segments, [rm_json:segment_detail(Segment, Total, Available) | JsonStruct]).

json_response(ReqData, State, Response) ->
    ReturnIo = mochijson2:encode(Response),
    ReturnJson = iolist_to_binary(ReturnIo),
    R2 = wrq:append_to_resp_body(ReturnJson, ReqData),
    {true, R2, State}.
    
show_resources(ReqData, State) ->
    case get_segment(ReqData) of
        undefined -> all_resources(ReqData, State);
        Segment ->
            SegmentStruct = get_segments_json([Segment], []),
            to_json(ReqData, State, rm_json:segments(SegmentStruct))
    end.

to_json(ReqData, State, Json) ->
    {mochijson2:encode(Json), ReqData, State}.

%%
%% Unit Tests
%%
-ifdef(EUNIT).

allowed_methods_test() -> {['GET', 'PUT'], reqdata, state} = allowed_methods(reqdata, state).
content_types_provided_test() -> {[{"application/json", to_json}], reqdata, state} = content_types_provided(reqdata, state).
content_types_accepted_test() -> {[{"application/json", from_json}], reqdata, state} = content_types_accepted(reqdata, state).
    
-endif.