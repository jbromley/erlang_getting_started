-module(tut8).
-export([reverse/1]).

% Exported functions

reverse(List) ->
    reverse(List, []).

% Module private functions

reverse([Head | Rest], Reversed_List) ->
    reverse(Rest, [Head | Reversed_List]);
reverse([], Reversed_List) ->
    Reversed_List.


