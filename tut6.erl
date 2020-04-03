-module(tut6).
-export([list_max/1]).

%% Exported functions

list_max([]) ->
    0;
list_max([Head | Rest]) ->
    list_max(Rest, Head).

%% Module private functions

list_max([], Res) ->
    Res;
list_max([Head | Rest], Current_Max) when Head > Current_Max ->
    list_max(Rest, Head);
list_max([_ | Rest], Current_Max) ->
    list_max(Rest, Current_Max).



