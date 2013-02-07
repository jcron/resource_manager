%% @author Jeremy Cron <jcron@virtualhold.com>
%% @copyright 2013 VirtualHold.
%% @doc Example webmachine_resource.

-module(resource_manager_resource).

-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, State) -> 
    {[ 'GET' ], ReqData, State }.

content_types_provided(ReqData, State) ->
    {[{"application/json",to_json}], ReqData, State}.

to_json(ReqData, State) ->
    try
        case wrq:path_info(action, ReqData) of
            "checkout" ->
                checkout_resource(ReqData, State);
            "checkin" ->
                checkin_resource(ReqData, State);
            undefined ->
                show_resources(ReqData, State)
        end
    catch
        no_resource -> to_json(ReqData, State, [{error, no_resource}]);
        _:_ -> to_json(ReqData, State, [{error, unknown_error}])
    end.

show_resources(ReqData, State) ->    
    case get_segment(ReqData) of
        undefined -> all_resources(ReqData, State);
        Segment ->
            SegmentStruct = get_segments_json([[Segment]], []),
            to_json(ReqData, State, [{segments, SegmentStruct}])
    end.

all_resources(ReqData, State) ->
    Segments = rm_librarian:get_all_segments(),
    SegmentStruct = get_segments_json(Segments, []),
    to_json(ReqData, State, [{segments, SegmentStruct}]).

checkin_resource(ReqData, State) ->
    Segment = get_segment(ReqData),
    Resources = rm_librarian:check_in_resource(Segment),
    SegmentStruct = get_segment_json(Segment, Resources),
    to_json(ReqData, State, [{segments, SegmentStruct}]).

checkout_resource(ReqData, State) ->
    Segment = get_segment(ReqData),
    Resources = rm_librarian:check_out_resource(Segment),
    SegmentStruct = get_segment_json(Segment, Resources),
    to_json(ReqData, State, [{segments, SegmentStruct}]).    

get_segment(ReqData) ->
    wrq:get_qs_value("Segment", ReqData).

get_segment_json(Segment, Resources) ->
    [{name, iolist_to_binary(Segment)}, {totalResources, rm_librarian:get_total_resources(Segment)}, {availableResources, Resources}].

get_segments_json([], JsonStruct) ->
    JsonStruct;
get_segments_json([Segment | Segments], JsonStruct) ->
    get_segments_json(Segment, Segments, JsonStruct).

get_segments_json([Segment | _], Segments, JsonStruct) ->
    Resources = rm_librarian:get_available_resources(Segment),
    get_segments_json(Segments, [get_segment_json(Segment, Resources) | JsonStruct]).

to_json(ReqData, State, Json) ->
    {mochijson2:encode(Json), ReqData, State}.

%%
%% Unit Tests
%%
-ifdef(EUNIT).

allowed_methods_test() -> {['GET'],reqdata,state} = allowed_methods(reqdata, state).
content_types_provided_test() -> {[{"application/json", to_json}], reqdata, state} = content_types_provided(reqdata, state).
    
-endif.