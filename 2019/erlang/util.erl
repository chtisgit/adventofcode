-module(util).
-export([getInputDirections/0]).
-export([getInputCommaInts/0]).
-export([getInputLineInts/0]).

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
