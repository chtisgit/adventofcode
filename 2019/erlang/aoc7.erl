-module(aoc7).
-export([main/0]).
-export([perms/1]).
b2i(true) -> 1;
b2i(_) -> 0.

d(I, RAM, 1) -> array:get(I, RAM);
d(I, RAM, 0) -> 
	I2 = array:get(I, RAM),
	array:get(I2, RAM).

d(I, RAM) -> d(I, RAM, 1).

r(RAM, PC, Amode, Bmode) ->
    {d(PC+1, RAM, Amode), d(PC+2, RAM, Bmode), d(PC+3, RAM)}.

vmexec({3,_,_}, RAM, PC, [], Outputs) ->
    {RAM, PC, [], Outputs, need_input};
vmexec({3,_,_}, RAM, PC, [I|Inputs], Outputs) ->
    StAddr = d(PC+1, RAM),
    {array:set(StAddr, I, RAM), PC + 2, Inputs, Outputs, running};
vmexec({4,_,Bmode}, RAM, PC, Inputs, Outputs) ->
    LdVal = d(PC+1, RAM, Bmode),
    {RAM, PC + 2, Inputs, [LdVal|Outputs], running};
vmexec({5,Amode,Bmode}, RAM, PC, Inputs, Outputs) ->
    P = d(PC+1, RAM, Amode),
    if P =/= 0 -> NextPC = d(PC+2, RAM, Bmode);
        true -> NextPC = PC + 3
    end,
    {RAM, NextPC, Inputs, Outputs, running};
vmexec({6,Amode,Bmode}, RAM, PC, Inputs, Outputs) ->
    P = d(PC+1, RAM, Amode),
    if P =:= 0 -> NextPC = d(PC+2, RAM, Bmode);
        true -> NextPC = PC + 3
    end,
    {RAM, NextPC, Inputs, Outputs, running};
vmexec({99,_,_}, RAM, PC, Inputs, Outputs) ->
    {RAM, PC, Inputs, Outputs, ended};
vmexec({Opcode, Amode, Bmode}, RAM, PC, Inputs, Outputs) ->
    {A, B, AddrC} = r(RAM, PC, Amode, Bmode),
    if Opcode =:= 1 -> Res = A+B;
       Opcode =:= 2 -> Res = A*B;
       Opcode =:= 7 -> Res = b2i(A < B);
       Opcode =:= 8 -> Res = b2i(A =:= B);
       true -> Res = error
    end,
    {array:set(AddrC, Res, RAM), PC + 4, Inputs, Outputs, running};
vmexec(Op,RAM,PC,Inputs, Outputs) ->
    io:format("error: bad opcode: ~p~n", [Op]),
    {RAM, PC, Inputs, Outputs, error}.

vm({RAM, PC, Inputs, Outputs, running}) ->
    Op = d(PC, RAM),
    vm(vmexec({Op rem 100, (Op div 100) rem 10, (Op div 1000) rem 10}, RAM, PC, Inputs, Outputs));
vm(State) -> State.

ampChain(_, [], Input) -> Input;
ampChain(RAM, [P|Phases], Input) ->
	{_, _, _, L, _} = vm({array:fix(array:from_list(RAM)), 0, [P,Input], [], running}),
	ampChain(RAM, Phases, lists:nth(1, L)).

ampLoop([], [], Inputs) -> lists:nth(1, Inputs);
ampLoop([{RAM,PC,_,_,_}|Rest], [], Inputs) ->
    {RAM2, PC2, _, Outputs, Status2} = vm({RAM,PC,Inputs,[],running}),
    if Status2 =:= ended ->
        ampLoop(Rest, [], Outputs);
       Status2 =:= need_input ->
        ampLoop(Rest ++ [{RAM2, PC2, [], [], running}], [], Outputs);
       true -> error
    end;
ampLoop([Machine|Rest], [P|Phases], Inputs) ->
    {RAM, PC, _, _, need_input} = vm(Machine),
    {RAM2, PC2, _, _, need_input} = vm({RAM, PC, [P], [], running}),
    ampLoop(Rest ++ [{RAM2, PC2, [], [], running}], Phases, Inputs).

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].

main() ->
    RAM = util:getInputCommaInts(),
    Perms = perms(lists:seq(0,4)),
    A = lists:max(lists:map(fun(Perm) ->
        ampChain(RAM, Perm, 0)
    end, Perms)),
    io:format("a: ~p~n", [A]),

    Perms2 = perms(lists:seq(5,9)),
    B = lists:max(lists:map(fun(Perm) ->
        ampLoop([{array:fix(array:from_list(RAM)), 0, [], [], running} || _ <- lists:seq(1,5)], Perm, [0])
    end, Perms2)),
    io:format("b: ~p~n", [B]).
