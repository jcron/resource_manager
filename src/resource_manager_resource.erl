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
    Segment = "Sales",
    try
        case wrq:path_info(action, ReqData) of
            "checkout" ->
                Resources = rm_librarian:check_out_resource(Segment),
                get_segment_json(ReqData, State, Segment, Resources);
            "checkin" ->
                Resources = rm_librarian:check_in_resource(Segment),
                get_segment_json(ReqData, State, Segment, Resources);
            undefined ->
                Resources = rm_librarian:get_available_resources(Segment),
                get_segment_json(ReqData, State, Segment, Resources)
        end
    catch
        no_resource -> to_json(ReqData, State, {struct, [{error, no_resource}]});
        _:_ -> to_json(ReqData, State, {struct, [{error, unknown_error}]})
    end.
    
to_json(ReqData, State, Struct) ->
    {mochijson2:encode(Struct), ReqData, State}.
    
%get_segment_json(ReqData, State, Resources) ->
%    Segments = rm_librarian:get_all_segments(),
%    get_segment_json(ReqData, State, Segments, Resources)
%    SegmentStruct = {struct, [{name, iolist_to_binary(Segment)}, {totalResources, rm_librarian:get_total_resources(Segment)}, {availableResources, Resources}]},
%    to_json(ReqData, State, {struct, [{segments, SegmentStruct}]}).

%get_segment_json(ReqData, State, [], Resources, Json) ->
%    {struct, [Json]}
%get_segment_json(ReqData, State, [Segment|Segments], Resources, Json) ->
%    NewJson = {struct, []},
%    get_segment_json(ReqData, State, Segments, Resources, Json);
    
get_segment_json(ReqData, State, Segment, Resources) ->
    SegmentStruct = {struct, [{name, iolist_to_binary(Segment)}, {totalResources, rm_librarian:get_total_resources(Segment)}, {availableResources, Resources}]},
    to_json(ReqData, State, {struct, [{segments, SegmentStruct}]}).

%%
%% Unit Tests
%%
-ifdef(EUNIT).

allowed_methods_test() -> {['GET'],reqdata,state} = allowed_methods(reqdata,state).
content_types_provided_test() -> {[{"application/json",to_json}],reqdata,state} = content_types_provided(reqdata,state).
    
-endif.