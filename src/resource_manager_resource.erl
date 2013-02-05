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
            "checkout" -> Resources = check_out_resource();
            "checkin" -> Resources = check_in_resource();
            undefined -> Resources = get_available_resources()
        end,
        to_json(ReqData, State, [{totalResources, get_total_resources()}, {availableResources, Resources}])
    catch
        no_resource -> to_json(ReqData, State, [{error, no_resource}])
    end.

to_json(ReqData, State, Struct) ->
    {mochijson2:encode({struct, Struct}), ReqData, State}.

check_out_resource() ->
    {_, Available} = rm_store:find(available_resources),
    update_available_resources(get_total_resources(), Available - 1).
    
check_in_resource() ->    
    {_, Available} = rm_store:find(available_resources),
    update_available_resources(get_total_resources(), Available + 1).
    
get_available_resources() ->
    {_, Available} = rm_store:find(available_resources),
    Available.
    
get_total_resources() ->
    {_, Total} = rm_store:find(total_resources),
    Total.

update_available_resources(_, Resources) when Resources < 0 ->
    throw(no_resource);
update_available_resources(Total, Resources) when Resources > Total ->
    throw(no_resource);
update_available_resources(_, Resources) ->
    rm_store:insert(available_resources, Resources),
    {_, Available} = rm_store:find(available_resources),
    Available.
    
%%
%% Unit Tests
%%
-ifdef(EUNIT).

allowed_methods_test() -> {['GET'],reqdata,state} = allowed_methods(reqdata,state).
content_types_provided_test() -> {[{"application/json",to_json}],reqdata,state} = content_types_provided(reqdata,state).

cannot_update_when_no_resources_left_test() ->
    ?assertThrow(no_resource, update_available_resources(0, -1)).

cannot_update_when_resources_is_more_than_total_test() ->
    ?assertThrow(no_resource, update_available_resources(1, 2)).

with_setup_of_storage_test_() ->
    {setup,
     fun setup/0,
     {inorder,
     [?_test(get_total_resources_returns_correct_count()),
      ?_test(check_out_resource_decreases_count()),
      ?_test(check_in_resource_increases_count())]}
    }.
    
get_total_resources_returns_correct_count() ->    
    ?assertEqual(1, get_total_resources()).

check_out_resource_decreases_count() ->
    ?assertEqual(0, check_out_resource()).
    
check_in_resource_increases_count() ->
    ?assertEqual(1, check_in_resource()).

setup() ->
    rm_store:init(),
    rm_store:insert(total_resources, 1),
    rm_store:insert(available_resources, 1).
    
-endif.
