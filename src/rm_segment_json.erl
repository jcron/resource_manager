-module(rm_segment_json).

-export([get/3
            ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

get(Segment, TotalResources, AvailableResources) ->
    [{name, iolist_to_binary(Segment)}, {totalResources, TotalResources}, {availableResources, AvailableResources}].

%%
%% Unit Tests
%%
-ifdef(EUNIT).

get_json_test() -> [{name, <<"Segment">>},{totalResources,15},{availableResources,14}] = get("Segment", 15, 14).
    
-endif.