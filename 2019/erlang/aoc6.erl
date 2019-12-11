-module(aoc6).
-export([main/0]).

trace(A, M, L) ->
    case maps:is_key(A, M) of
       true -> trace(maps:get(A,M), M, [A|L]);
       false -> ["COM" | lists:droplast(L)]
    end.

countOrbits(A, M) -> length(trace(A, M, [])).

checksum([], _, _, Sum) -> Sum;
checksum([{A,_}|T], M, Counted, Sum) ->
    case maps:is_key(A, Counted) of
       true -> checksum(T, M, Counted, Sum);
       false ->
        checksum(T, M, maps:put(A, ok, Counted), Sum + countOrbits(A, M))
    end.

both(L1,L2) ->
    M = maps:from_list(lists:zip(L1, [ok || _ <- lists:seq(1,length(L1))])),
    lists:filter(fun(X) -> maps:is_key(X,M) end, L2).

findFirst([X|T],X,C,_) -> findFirst(T,X,C+1,C);
findFirst([_|T],X,C,A) -> findFirst(T,X,C+1,A);
findFirst([],_,_,A) -> A.

main() ->
    L = util:getInputOrbits(),
    M = maps:from_list(L),
    io:format("a: ~p~n", [checksum(L, M, maps:new(), 0)]),
    YOU = trace("YOU", M, []),
    SAN = trace("SAN", M, []),
    Intersect = both(YOU,SAN),
    Best = lists:nth(length(Intersect)-1, Intersect),
    N1 = length(YOU) - findFirst(YOU, Best, 0, 0),
    N2 = length(SAN) - findFirst(SAN, Best, 0, 0),
    io:format("b: ~p~n", [N1+N2-4]).

