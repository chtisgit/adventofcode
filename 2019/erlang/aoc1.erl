-module(aoc1).
-export([main/0]).

fuel(Mass) ->
    lists:max([(Mass div 3) - 2, 0]).

acc(Count, []) ->
    Count;
acc(Count, [H|T]) ->
    acc(Count + fuel(H), T).

acc2(Count, [], 0) ->
    Count;
acc2(Count, [H|T], 0) ->
    F = fuel(H),
    acc2(Count + F, T, F);
acc2(Count, L, M) ->
    F = fuel(M),
    acc2(Count + F, L, F).

intList() ->
    intList([]).
intList(L) ->
    {N,_} = string:to_integer(io:get_line(standard_io, "> ")),
    if N =:= error -> L;
       true -> intList([N|L])
    end.

main() ->
    L = intList(),
    io:format("a: ~.B~nb: ~.B~n", [acc(0, L), acc2(0,L,0)]).

