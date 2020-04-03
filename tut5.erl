-module(tut5).
-export([format_temps/1]).

%% Exported functions

format_temps([]) ->
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

%% Module-private functions

convert_to_celsius({Name, {c, Temp}}) -> % No conversion needed.
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) -> % Convert F to C.
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).


