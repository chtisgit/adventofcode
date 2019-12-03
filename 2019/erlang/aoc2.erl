-module(aoc2).
-export([main/0]).

d(I, L) ->
    lists:nth(I+1, L).

r(RAM, PC) ->
    {d(PC+1, RAM), d(PC+2, RAM), d(PC+3, RAM)}.

vmexec(1, RAM, PC) ->
    {AddrA, AddrB, AddrC} = r(RAM, PC),
    A = d(AddrA, RAM),
    B = d(AddrB, RAM),
    {lists:sublist(RAM,AddrC) ++ [A+B] ++ lists:nthtail(AddrC+1,RAM), PC + 4};
vmexec(2, RAM, PC) ->
    {AddrA, AddrB, AddrC} = r(RAM, PC),
    A = d(AddrA, RAM),
    B = d(AddrB, RAM),
    {lists:sublist(RAM,AddrC) ++ [A*B] ++ lists:nthtail(AddrC+1,RAM), PC + 4};
vmexec(99, RAM, _) ->
    {RAM, ended};
vmexec(_,RAM,_) ->
    {RAM, error}.

vm(RAM, PC) when is_atom(PC) -> {RAM,PC};
vm(RAM, PC) ->
    {RAM2, PC2} = vmexec(d(PC, RAM), RAM, PC),
    vm(RAM2, PC2).

res({_,error}) -> error;
res({RAM,_}) -> d(0, RAM).

insertCode(RAM, Noun, Verb) ->
    lists:sublist(RAM,1) ++ [Noun, Verb] ++ lists:nthtail(3,RAM).

a(RAM) ->
    VMRes = vm(insertCode(RAM, 12, 2), 0),
    {_, PC} = VMRes,
    io:format("a: ~p ~p~n", [PC, res(VMRes)]),
    PC.

b(_, Noun, Verb, Expect, Expect) ->
    {Noun,Verb};
b(_, 99, 100, _, _) ->
    {error,error};
b(RAM, Noun, 100, Expect, _) ->
    b(RAM, Noun+1, 0, Expect);
b(RAM, Noun, Verb, Expect, _) ->
    b(RAM, Noun, Verb+1, Expect).

b(RAM, Noun, Verb, Expect) ->
    Res = res(vm(insertCode(RAM, Noun, Verb), 0)),
    b(RAM, Noun, Verb, Expect, Res).

b(RAM) ->
    {Noun, Verb} = b(RAM, 0, 0, 19690720),
    io:format("b: ~p ~p ~n", [Noun, Verb]).

main() ->
    RAM = util:getInputCommaInts(),
    a(RAM),
    b(RAM).

