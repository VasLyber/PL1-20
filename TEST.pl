copy(L,R) :- accCp(L,R).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).

push([H|T],NewState1,State2,NewState2):-
  copy(State2,NState2),
  append(H,NState2,NewState2),
  copy(T,NewState1),
  writeln(NewState2),
  writeln(NewState1)
  .
