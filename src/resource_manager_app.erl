%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the resource_manager application.

-module(resource_manager_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for resource_manager.
start(_Type, _StartArgs) ->
    rm_store:init(),
    initialize_resources(),
    resource_manager_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for resource_manager.
stop(_State) ->
    ok.

initialize_resources() ->
    {_, Segments} = application:get_env(resource_manager, segments),
    {_, TotalResources} = application:get_env(resource_manager, total_resources),
    initialize_resources(Segments, TotalResources).
    
initialize_resources([], []) ->
    ok;
initialize_resources([Segment | SegmentTail], [Resources | ResourcesTail]) ->
    rm_store:insert({Segment, total_resources}, list_to_integer(Resources)),
    {_, Available} = rm_store:find({Segment, total_resources}),
    rm_store:insert({Segment, available_resources}, Available),
    initialize_resources(SegmentTail, ResourcesTail).
