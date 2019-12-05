-module(aoc5).
-export([main/0]).

d(I, L, 1) -> lists:nth(I+1,L);
d(I, L, 0) -> lists:nth(lists:nth(I+1, L)+1, L).

d(I, L) -> d(I, L, 1).

r(RAM, PC, Amode, Bmode) ->
    {d(PC+1, RAM, Amode), d(PC+2, RAM, Bmode), d(PC+3, RAM)}.

vmexec({3,_,_}, RAM, PC) ->
    I = util:getInputInt(),
    StAddr = d(PC+1, RAM),
    io:format("StAddr: ~p  I=~p~n", [StAddr,I]),
    {lists:sublist(RAM,StAddr) ++ [I] ++ lists:nthtail(StAddr+1,RAM), PC + 2};
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
    io:format("Op: ~p A(Mode=~p): ~p B(Mode=~p): ~p AddrC: ~p~n", [Opcode,Amode,A,Bmode,B,AddrC]),
    if Opcode =:= 1 -> Res = A+B;
       Opcode =:= 2 -> Res = A*B;
       Opcode =:= 7 andalso A < B -> Res = 1;
       Opcode =:= 7 -> Res = 0;
       Opcode =:= 8 andalso A =:= B -> Res = 1;
       Opcode =:= 8 -> Res = 0;
       true -> Res = error
    end,
    {lists:sublist(RAM,AddrC) ++ [Res] ++ lists:nthtail(AddrC+1,RAM), PC + 4};
vmexec(Op,RAM,_) ->
    io:format("Bad? Opcode: ~p~n", [Op]),
    {RAM, error}.

vm(RAM, PC) when is_atom(PC) -> {RAM,PC};
vm(RAM, PC) ->
    Op = d(PC, RAM),
    {RAM2, PC2} = vmexec({Op rem 100, (Op div 100) rem 10, (Op div 1000) rem 10}, RAM, PC),
    vm(RAM2, PC2).

main() ->
    RAM = util:getInputCommaInts(),
    vm(RAM, 0).

