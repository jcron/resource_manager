-module(rm_json).

-export([error/1,
              segment_detail/3,
              segments/1
            ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

error(Type) ->
    [{error, Type}].
    
segment_detail(Segment, TotalResources, AvailableResources) ->
    [{name, iolist_to_binary(Segment)}, {totalResources, TotalResources}, {availableResources, AvailableResources}].
    
segments(Segments) ->
    [{segments, Segments}].

%%
%% Unit Tests
%%
-ifdef(EUNIT).

error_json_test() -> [{error, no_resource}] = rm_json:error(no_resource).
segment_detail_json_test() -> [{name, <<"Segment">>},{totalResources,15},{availableResources,14}] = segment_detail("Segment", 15, 14).
segments_json_test() -> [{segments, [{name, <<"Segment">>},{totalResources,2},{availableResources,1}] }]= segments(segment_detail("Segment", 2, 1)).
    
-endif.