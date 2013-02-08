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
    initialize_resources_from_config(),
    resource_manager_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for resource_manager.
stop(_State) ->
    ok.

%%% Local Functions
initialize_resources([]) ->
    ok;
initialize_resources([{Segment, Total} | RestOfResources]) ->
    rm_store:initialize_segment(Segment, Total),
    initialize_resources(RestOfResources).

initialize_resources_from_config() ->
    {_, Resources} = application:get_env(resource_manager, resources),
    initialize_resources(Resources).