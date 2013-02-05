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
    resource_manager_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for resource_manager.
stop(_State) ->
    ok.
