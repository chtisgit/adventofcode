-module(aoc3).
-export([main/0]).

expand([{_,0}|Rest], Pos, Positions) ->
    expand(Rest, Pos, Positions);
expand([{Dir,N}|Rest], {X,Y}, Positions) ->
    if Dir =:= "L" -> New = {X-1,Y};
       Dir =:= "R" -> New = {X+1,Y};
       Dir =:= "U" -> New = {X,Y-1};
       Dir =:= "D" -> New = {X,Y+1};
       true -> New = error end,
    expand([{Dir,N-1}|Rest], New, [{X,Y}|Positions]);
expand([], Pos, Positions) ->
    [Pos|Positions].

both(L1,L2) ->
    % using a map for (a few magnitudes high) performance optimization
    M = maps:from_list(lists:zip(L1, [ok || _ <- lists:seq(1,length(L1))])),
    lists:filter(fun(X) -> maps:is_key(X,M) andalso X =/= {0,0} end, L2).

dist({X,Y}) -> abs(X)+abs(Y).

findFirst([X|T],X,C,_) -> findFirst(T,X,C+1,C);
findFirst([_|T],X,C,A) -> findFirst(T,X,C+1,A);
findFirst([],_,_,A) -> A.

main() ->
    Wire1 = expand(util:getInputDirections(), {0,0}, []),
    Wire2 = expand(util:getInputDirections(), {0,0}, []),
    io:format("wire1 length: ~p~nwire2 length: ~p~n~n", [length(Wire1), length(Wire2)]),

    % not so slow anymore :)
    B = both(Wire1,Wire2),

    Min = lists:min(lists:map(fun(Pos) -> dist(Pos) end, B)),
    io:format("a: ~p~n", [Min]),
    
    MinSteps = lists:min(lists:map(fun(Pos) ->
            length(Wire1) - findFirst(Wire1, Pos, 1, error) +
            length(Wire2) - findFirst(Wire2, Pos, 1, error)
        end, B)),
    io:format("b: ~p~n", [MinSteps]).