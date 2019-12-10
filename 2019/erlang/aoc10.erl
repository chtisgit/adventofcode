-module(aoc10).
-export([main/0]).

chr([H|_]) -> H;
chr(_) -> error.

is_mult({X,Y}, {BX, BY}) ->
    N = X div BX,
    if Y =:= N * BY -> true;
       true -> false
    end.

is_multiple({X,Y}, {BX, BY}) when BX =/= 0 andalso X rem BX =:= 0 -> is_mult({X,Y},{BX,BY});
is_multiple({X,Y}, {0, BY}) -> is_mult({Y,X}, {BY,0});
is_multiple(_, _) -> false.

norm({X,Y}) ->
    Gcd = util:gcd(X,Y),
    {X div Gcd, Y div Gcd}.

count(_, _, [], Acc) -> Acc;
count(M, {X,Y}, [{AX,AY}|Rest], Acc) ->
    case maps:is_key({X+AX,Y+AY}, M) andalso maps:get({X+AX, Y+AY}, M) of
    asteroid ->
        N = lists:filter(fun(C) -> not is_multiple(C, norm({AX,AY})) end, Rest),
        count(M, {X,Y}, N, Acc+1);
    _ ->
        count(M, {X,Y}, Rest, Acc)
    end.

dist({X,Y},{X2,Y2}) ->
    (X-X2)*(X-X2) + (Y-Y2)*(Y-Y2).

main() ->
    M = util:getMap(maps:from_list([{chr("."),nothing},{chr("#"),asteroid}])),
    Orig = {3,4},
    Vecs = lists:filter(fun
        ({0,0}) -> false; (_) -> true
    end, [{X,Y} || X <- lists:seq(-48,48), Y <- lists:seq(-48, 48)]),
    ListA = maps:to_list(
        maps:map(fun(Coord, asteroid) -> count(M, Coord, lists:sort(fun(A,B) -> 
            dist(A,Orig) =< dist(B,Orig)
        end, Vecs), 0);
        (_, _) -> 0 end, M)),
    A = util:max(fun({_, Val1}, {_, Val2}) -> Val1 =< Val2 end, ListA),
    io:format("a: ~p~n", [A]).