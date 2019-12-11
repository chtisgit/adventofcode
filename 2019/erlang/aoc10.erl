-module(aoc10).
-export([main/0]).
-export([is_multiple/2]).

chr([H|_]) -> H;
chr(_) -> error.

% don't take into account negative multiples because that is
% considered a different direction.
is_multiple({X,Y}, {BX,BY}) -> norm({X,Y}) =:= norm({BX,BY}).

norm({X,Y}) ->
    Gcd = abs(util:gcd(X,Y)),
    {X div Gcd, Y div Gcd}.

count(_, _, [], Acc) -> Acc;
count(M, {X,Y}, [{AX,AY}|Rest], Acc) ->
    case maps:is_key({X+AX,Y+AY}, M) andalso maps:get({X+AX, Y+AY}, M) of
    asteroid ->
        Normed = norm({AX,AY}),
        N = lists:filter(fun(C) -> not is_multiple(C, Normed) end, Rest),
        count(M, {X,Y}, N, Acc+1);
    _ ->
        count(M, {X,Y}, Rest, Acc)
    end.

main() ->
    M = util:getMap(maps:from_list([{chr("."),nothing},{chr("#"),asteroid}])),
    Vecs = [{X,Y} || X <- lists:seq(-48,48), Y <- lists:seq(-48, 48), X =/= 0 orelse Y =/= 0],
    ListA = maps:to_list(
        maps:map(fun
            (Coord, asteroid) -> count(M, Coord, Vecs, 0);
            (_, _) -> 0 end, M)),
    A = util:max(fun({_, Val1}, {_, Val2}) -> Val1 =< Val2 end, ListA),
    io:format("a: ~p~n", [A]).
