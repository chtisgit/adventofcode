-module(aoc1).
-export([main/0]).

fuel(Mass) -> lists:max([(Mass div 3) - 2, 0]).

fuel2(0, A) -> A;
fuel2(Mass, A) -> fuel2(fuel(Mass), A+fuel(Mass)).

acc(L) -> lists:sum(lists:map(fun(X) -> fuel(X) end, L)).

acc2(L) -> lists:sum(lists:map(fun(X) -> fuel2(X, 0) end, L)).

intList() ->
    intList([]).
intList(L) ->
    {N,_} = string:to_integer(io:get_line(standard_io, "> ")),
    if N =:= error -> L;
       true -> intList([N|L])
    end.

main() ->
    L = intList(),
    io:format("a: ~.B~nb: ~.B~n", [acc(L), acc2(L)]).

