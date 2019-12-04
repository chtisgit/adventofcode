-module(aoc4).
-export([main/0]).

ascending(L) ->
    is_number(lists:foldl(fun
        (_, Prev) when is_atom(Prev) -> Prev;
        (X,Prev) when (X < Prev) -> false;
        (X,_) -> X end, 0, L)).

% count consecutive numbers returns {Digit, #Occurrences}
coco(X,[{X,N}|T]) -> [{X,N+1}|T];
coco(X,R) -> [{X,1}|R].

adjacentSame(L, From, To) ->
    lists:any(fun
            ({_,N}) when ((From =:= ok orelse N >= From) andalso (To =:= ok orelse N =< To)) -> true;
            (_) -> false
        end, lists:foldl(fun coco/2, [], L)).

pow10(I) -> lists:nth(I+1,[1,10,100,1000,10000,100000]).

dig(N,I) -> (N div pow10(I)) rem 10.

b2i(true) -> 1;
b2i(_) -> 0.

criteria(N) ->
    L = [dig(N,I) || I <- [5,4,3,2,1,0]],
    Ascending = ascending(L),
    {b2i(Ascending andalso adjacentSame(L, 2, ok)),
     b2i(Ascending andalso adjacentSame(L, 2, 2))}.

main() ->
    {From, To} = util:getIntRange(),
    AB = [criteria(N) || N <- lists:seq(From, To)],
    {A,B} = lists:foldl(fun
        ({X,Y}, {XPrev, YPrev}) -> {X+XPrev, Y+YPrev}
        end, {0,0}, AB),
    io:format("a: ~p~nb: ~p~n~n", [A,B]).