-module(rm_json).

-export([error/1,
              connection_detail/3,
              connections/1
            ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

error(Type) ->
    [{error, Type}].
    
connection_detail(Connection, TotalResources, AvailableResources) ->
    [{name, iolist_to_binary(Connection)}, {totalResources, TotalResources}, {availableResources, AvailableResources}].
    
connections(Connections) ->
    [{connections, Connections}].

%%
%% Unit Tests
%%
-ifdef(EUNIT).

error_json_test() -> [{error, no_resource}] = rm_json:error(no_resource).
connection_detail_json_test() -> [{name, <<"Connection">>},{totalResources,15},{availableResources,14}] = connection_detail("Connection", 15, 14).
connections_json_test() -> [{connections, [{name, <<"Connection">>},{totalResources,2},{availableResources,1}] }]= connections(connection_detail("Connection", 2, 1)).
    
-endif.