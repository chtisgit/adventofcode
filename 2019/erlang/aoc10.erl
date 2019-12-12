-module(aoc10).
-export([main/0]).

chr([H|_]) -> H;
chr(_) -> error.

% don't take into account negative multiples because that is
% considered a different direction.
is_multiple({X,Y}, {BX,BY}) -> norm({X,Y}) =:= norm({BX,BY}).

norm({X,Y}) ->
    Gcd = abs(util:gcd(X,Y)),
    {X div Gcd, Y div Gcd}.

atan2(Y,X) ->
    V = math:atan2(Y,X),
    if V < 0 -> V+100;
       true -> V
    end.

clockwise({AX,AY},{BX,BY}) -> atan2(AX,-AY) =< atan2(BX,-BY).

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

dist0({X,Y}) -> abs(X)+abs(Y).

binAngles(L) -> lists:reverse(binAngles(L, [])).
binAngles([], Out) -> Out;
binAngles([V|Vecs], Out) ->
    {Bin,Rest} = lists:partition(fun(C) -> is_multiple(C,V) end,[V|Vecs]),
    SortedBin = lists:sort(fun(A,B) -> dist0(A) =< dist0(B) end, Bin),
    binAngles(Rest, [SortedBin|Out]).

vaporize(M, {BX,BY}, Count, AngleVecs) ->
    Res = vap(Count, lists:filtermap(fun(Bin) ->
        Bin2 = lists:filter(fun({AX,AY}) ->
            {X,Y} = {AX+BX, AY+BY},
            case maps:is_key({X,Y},M) andalso maps:get({X,Y},M) of
            asteroid -> true;
            _ -> false
            end
        end, Bin),
        if length(Bin2) =:= 0 -> false;
           true -> {true, Bin2}
        end
    end, AngleVecs), error),
    if is_atom(Res) -> Res;
       true ->
	{X,Y} = Res,
        {X+BX,Y+BY}
    end.

vap(0, _, Last) -> Last;
vap(_, [], _) -> error;
vap(Count, [CA|Rest], _) ->
    [Current|CARest] = CA,
    if length(CARest) =:= 0 -> L = Rest;
       true -> L = Rest ++ [CARest]
    end,
    vap(Count-1, L, Current).

main() ->
    M = util:getMap(maps:from_list([{chr("."),nothing},{chr("#"),asteroid}])),
    Vecs = [{X,Y} || X <- lists:seq(-48,48), Y <- lists:seq(-48, 48), X =/= 0 orelse Y =/= 0],
    ListA = maps:to_list(
        maps:map(fun
            (Coord, asteroid) -> count(M, Coord, Vecs, 0);
            (_, _) -> 0 end, M)),
    {ACoord,AValue} = util:max(fun({_, Val1}, {_, Val2}) -> Val1 =< Val2 end, ListA),
    io:format("a: ~p~n", [AValue]),
    AngleVecs = lists:sort(fun([H1|_],[H2|_]) -> clockwise(H1,H2) end, binAngles(Vecs)),
    B = vaporize(M, ACoord, 200, AngleVecs),
    io:format("b: ~p~n", [B]).
