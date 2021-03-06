-module(util).
-export([getInputDirections/0]).
-export([getInputCommaInts/0]).
-export([getInputLineInts/0]).
-export([getIntRange/0]).
-export([getInputInt/0]).
-export([getInputOrbits/0]).
-export([getMap/1]).
-export([max/2]).
-export([gcd/2]).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

max(Leq, L) -> max(Leq, L, error).

max(_, [], Max) -> Max;
max(Leq, [H|T], Max) ->
    case Max =/= error andalso Leq(H, Max) of
    true -> max(Leq, T, Max);
    false -> max(Leq, T, H)
    end.

min(Leq, List, Min) ->
    max(fun(A,B) -> not Leq(A,B) end, List, Min).

getInputInt() ->
    {N,_} = string:to_integer(io:get_line(standard_io, "> ")),
    N.

getInputDirections() ->
    I = io:get_line(standard_io, "> "),
    lists:map(fun(X) -> {Dir, S} = string:take(X, "RDLU"),
                {Int, _} = string:to_integer(S),
                {Dir, Int} end, 
            string:tokens(I, ",")).

getInputCommaInts() ->
    I = io:get_line(standard_io, "> "),
    lists:map(fun(X) -> {Int, _} = string:to_integer(X), 
                        Int end, 
            string:tokens(I, ",")).

getInputLineInts() ->
    getInputLineInts([]).

getInputLineInts(L) ->
    {N,_} = string:to_integer(io:get_line(standard_io, "> ")),
    if N =:= error -> L;
       true -> getInputLineInts([N|L])
    end.

trimList(L) ->
    lists:map(fun(X) -> string:trim(X) end, L).

getInputOrbits() ->
    getInputOrbits([]).

getInputOrbits(L) ->
    I = io:get_line(standard_io, "> "),
    T = trimList(string:tokens(I, ")")),
    if length(T) =:= 2 ->
       {A,B} = list_to_tuple(T),
       getInputOrbits([{B,A}|L]);
       true -> L
    end.

getIntRange() ->
    I = io:get_line(standard_io, "> "),
    erlang:list_to_tuple(
        lists:sublist(
            lists:map(fun(X) -> {Int, _} = string:to_integer(X), 
                            Int end, 
                string:tokens(I, "-")), 2)).

getMap(Mapping) ->
    getMap(Mapping, maps:new(), {0,-1}, []).

getMap(Mapping, M, {X,Y}, [H|T]) ->
    M2 = maps:put({X,Y}, maps:get(H, Mapping), M),
    getMap(Mapping, M2, {X+1,Y}, T);
getMap(Mapping, M, {_,Y}, []) ->
    I = string:trim(io:get_line(standard_io, "> ")),
    if length(I) =:= 0 -> M;
       true -> getMap(Mapping, M, {0,Y+1}, I)
    end.
