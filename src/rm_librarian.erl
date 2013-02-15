-module(rm_librarian).

-export([check_out_resource/1,
         check_in_resource/1,
         get_all_segments/0,
         get_available_resources/1,
         get_total_resources/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

check_out_resource(Segment) ->
    Available = rm_store:find({Segment, available_resources}),
    update_available_resources(Segment, get_total_resources(Segment), Available - 1).

check_in_resource(Segment) ->    
    Available = rm_store:find({Segment, available_resources}),
    update_available_resources(Segment, get_total_resources(Segment), Available + 1).

get_all_segments() ->
    rm_store:find_all(total_resources).

get_available_resources(Segment) ->
    rm_store:find({Segment, available_resources}).

get_total_resources(Segment) ->
    rm_store:find({Segment, total_resources}).

%%% Local Functions
update_available_resources(_, _, Resources) when Resources < 0 ->
    throw(no_resource);
update_available_resources(_, Total, Resources) when Resources > Total ->
    throw(no_resource);
update_available_resources(Segment, Total, Resources) ->
    rm_store:insert({Segment, available_resources}, Resources),
    {Total, rm_store:find({Segment, available_resources})}.

%%
%% Unit Tests
%%
-ifdef(EUNIT).

cannot_update_when_no_resources_left_test() ->
    ?assertThrow(no_resource, update_available_resources("Sales", 0, -1)).

cannot_update_when_resources_is_more_than_total_test() ->
    ?assertThrow(no_resource, update_available_resources("Sales", 1, 2)).
    
with_setup_of_storage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun instantiator/1
    }.

get_total_resources_returns_correct_count(Segment) ->
    ?assertEqual(1, get_total_resources(Segment)).
    
check_out_resource_decreases_count(Segment) ->
    ?assertEqual({1, 0}, check_out_resource(Segment)).

check_in_resource_increases_count(Segment) ->
    ?assertEqual({1, 1}, check_in_resource(Segment)).

get_all_segments_returns_all_segments(Segment) ->
    ?assertEqual([Segment], get_all_segments()).

setup() ->
    rm_store:init(),
    rm_store:initialize_segment("Sales", 1).

cleanup(_) ->
    rm_store:finalize().

instantiator(Segment) ->
    {inorder,
        [?_test(get_total_resources_returns_correct_count(Segment)),
         ?_test(check_out_resource_decreases_count(Segment)),
         ?_test(check_in_resource_increases_count(Segment)),
         ?_test(get_all_segments_returns_all_segments(Segment))]
    }.

-endif.
