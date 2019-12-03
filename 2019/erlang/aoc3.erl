-module(aoc3).
-export([main/0]).

dirList() ->
    I = io:get_line(standard_io, "> "),
    lists:map(fun(X) -> {Dir, S} = string:take(X, "RDLU"),
                {Int, _} = string:to_integer(S),
                {Dir, Int} end, 
            string:tokens(I, ",")).

expand([{_,0}|Rest], Pos, Positions) ->
    expand(Rest, Pos, Positions);
expand([{"R",N}|Rest], {X,Y}, Positions) ->
    expand([{"R",N-1}|Rest], {X+1,Y}, [{X,Y}|Positions]);
expand([{"L",N}|Rest], {X,Y}, Positions) ->
    expand([{"L",N-1}|Rest], {X-1,Y}, [{X,Y}|Positions]);
expand([{"U",N}|Rest], {X,Y}, Positions) ->
    expand([{"U",N-1}|Rest],  {X,Y-1}, [{X,Y}|Positions]);
expand([{"D",N}|Rest], {X,Y}, Positions) ->
    expand([{"D",N-1}|Rest], {X,Y+1}, [{X,Y}|Positions]);
expand([], Pos, Positions) ->
    [Pos|Positions].

both(L1,L2) ->
    lists:filter(fun(X) -> lists:member(X,L1) andalso X =/= {0,0} end, L2).

dist({X,Y}) -> abs(X)+abs(Y).

% not sure if this is delivers the correct value, but it worked...
findFirst([X|_],X,C) -> C;
findFirst([_|T],X,C) -> findFirst(T,X,C+1);
findFirst([],_,_) -> error. 

main() ->
    Wire1 = expand(dirList(), {0,0}, []),
    Wire2 = expand(dirList(), {0,0}, []),
    io:format("wire1 length: ~p~nwire2 length: ~p~n~n", [length(Wire1), length(Wire2)]),

    % this is extremely slow ... :(
    B = both(Wire1,Wire2),

    Min = lists:min(lists:map(fun(Pos) -> dist(Pos) end, B)),
    io:format("a: ~p~n", [Min]),
    
    MinSteps = lists:min(lists:map(fun(Pos) ->
            length(Wire1) - findFirst(Wire1, Pos, 1) + 
            length(Wire2) - findFirst(Wire2, Pos, 1)
        end, B)),
    io:format("b: ~p~n", [MinSteps]).