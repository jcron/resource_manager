-module(rm_librarian).

-export([check_out_resource/2,
             check_in_resource/2,
             get_all_segments/0,
             get_available_resources/1,
             get_total_resources/1
            ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

check_out_resource(_, []) ->
    throw(no_resource);
check_out_resource([], _) ->
    throw(no_resource);
check_out_resource(Segment, Conversation) ->
    Available = rm_store:find({Segment, available_resources}),
    case rm_store:conversation_exists(Conversation, available_resources) of
        true  -> throw(no_resource);
        false ->
            rm_store:add_conversation(Conversation, available_resources),
            update_available_resources(Segment, get_total_resources(Segment), Available - 1)
    end.    

check_in_resource(_, []) ->
    throw(no_resource);
check_in_resource([], _) ->
    throw(no_resource);
check_in_resource(Segment, Conversation) ->
    Available = rm_store:find({Segment, available_resources}),
    case rm_store:conversation_exists(Conversation, available_resources) of
        true  ->
            rm_store:remove_conversation(Conversation),
            update_available_resources(Segment, get_total_resources(Segment), Available + 1);
        false -> throw(no_resource)
    end.  

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

check_in_needs_a_conversation_test() ->
    ?assertThrow(no_resource, check_in_resource("Sales", [])).
    
check_in_needs_a_segment_test() ->
    ?assertThrow(no_resource, check_in_resource("Sales", [])).

check_out_needs_a_conversation_test() ->
    ?assertThrow(no_resource, check_out_resource([], "id")).

check_out_needs_a_segment_test() ->
    ?assertThrow(no_resource, check_out_resource([], "id")).

with_setup_of_storage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun instantiator/1
    }.

get_total_resources_returns_correct_count(Segment) ->
    ?assertEqual(1, get_total_resources(Segment)).

check_out_resource_decreases_count(Segment) ->
    ?assertEqual({1, 0}, check_out_resource(Segment, "id")).

check_out_only_allows_one_resource_per_id(Segment) ->
    ?assertThrow(no_resource, check_out_resource(Segment, "id")).
    
check_in_resource_increases_count(Segment) ->
    ?assertEqual({1, 1}, check_in_resource(Segment, "id")).

check_in_resource_needs_id_already_checked_out(Segment) ->
    ?assertThrow(no_resource, check_in_resource(Segment, "id")).
    
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
         ?_test(check_out_only_allows_one_resource_per_id(Segment)),
         ?_test(check_in_resource_increases_count(Segment)),
         ?_test(check_in_resource_needs_id_already_checked_out(Segment)),
         ?_test(get_all_segments_returns_all_segments(Segment))]
    }.

-endif.
