-module(test).
-export([mod/2, is_odd/1]).

mod(X, Y) ->
    (X rem Y + Y) rem Y.

is_odd(X) ->
    X rem 2 == 1.
