-module(aoc5).
-export([main/0]).

b2i(true) -> 1;
b2i(_) -> 0.

d(I, RAM, 1) -> array:get(I, RAM);
d(I, RAM, 0) -> array:get(array:get(I, RAM), RAM).

d(I, RAM) -> d(I, RAM, 1).

r(RAM, PC, Amode, Bmode) ->
    {d(PC+1, RAM, Amode), d(PC+2, RAM, Bmode), d(PC+3, RAM)}.

vmexec({3,_,_}, RAM, PC) ->
    I = util:getInputInt(),
    StAddr = d(PC+1, RAM),
    {array:set(StAddr, I, RAM), PC + 2};
vmexec({4,_,Bmode}, RAM, PC) ->
    LdVal = d(PC+1, RAM, Bmode),
    io:format("~p~n", [LdVal]),
    {RAM, PC + 2};
vmexec({5,Amode,Bmode}, RAM, PC) ->
    P = d(PC+1, RAM, Amode),
    if P =/= 0 -> {RAM, d(PC+2, RAM, Bmode)};
        true -> {RAM, PC+3}
    end;
vmexec({6,Amode,Bmode}, RAM, PC) ->
    P = d(PC+1, RAM, Amode),
    if P =:= 0 -> {RAM, d(PC+2, RAM, Bmode)};
        true -> {RAM, PC+3}
    end;
vmexec({99,_,_}, RAM, _) ->
    {RAM, ended};
vmexec({Opcode, Amode, Bmode}, RAM, PC) ->
    {A, B, AddrC} = r(RAM, PC, Amode, Bmode),
    if Opcode =:= 1 -> Res = A+B;
       Opcode =:= 2 -> Res = A*B;
       Opcode =:= 7 -> Res = b2i(A < B);
       Opcode =:= 8 -> Res = b2i(A =:= B);
       true -> Res = error
    end,
    {array:set(AddrC, Res, RAM), PC + 4};
vmexec(Op,RAM,_) ->
    io:format("error: bad opcode: ~p~n", [Op]),
    {RAM, error}.

vm(RAM, PC) when is_atom(PC) -> {RAM,PC};
vm(RAM, PC) ->
    Op = d(PC, RAM),
    {RAM2, PC2} = vmexec({Op rem 100, (Op div 100) rem 10, (Op div 1000) rem 10}, RAM, PC),
    vm(RAM2, PC2).

main() ->
    RAM = util:getInputCommaInts(),
    {_, PC} = vm(array:fix(array:from_list(RAM)), 0),
    PC.
