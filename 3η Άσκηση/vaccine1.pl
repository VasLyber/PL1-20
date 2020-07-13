reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

complement([],Z,Y):- reverse(Y,Z,[]).
complement([H|T],Z,Acc) :-
  (
    H == -25 ->  complement(T,Z,[-29|Acc])
  ; H == -29 ->  complement(T,Z,[-25|Acc])
  ; H == -31 ->  complement(T,Z,[-11|Acc])
  ; H == -11 ->  complement(T,Z,[-31|Acc])
  ).

sum_atoms([], []).
sum_atoms([H|T], [HeadSum|RT]) :-
  atom_codes(H, Codes),
  sum_letters(Codes, HeadSum),
  sum_atoms(T, RT).

sum_letters([], 0).
sum_letters([H|T], Res) :-
  sum_letters(T, TailSum),
  Res is (H-96) + TailSum.

copy(L,R) :- accCp(L,R).
accCp([],[]).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).

push([H|T],NewState1,State2,NewState2):-
  copy(State2,NState2),
  append([H],NState2,NewState2),
  copy(T,NewState1).

action(State1,State2,c,NewState1,NewState2):-
  copy(State2,NewState2),
  complement(State1,NewState1,[]).
action(State1,State2,r,NewState1,NewState2):-
  copy(State1,NewState1),
  reverse(State2,NewState2,[]).
action(State1,State2,p,NewState1,NewState2):-
  push(State1,NewState1,State2,NewState2).

all_diff(L) :- safe([],K1),\+ (append(_,[X|R],L), memberchk(X,R)).
safe([],K).
safe([H],K):-
  K1 = [H|K],
  all_diff(K1).
safe([H1,H2|T],K):-
  (
    H1 \= H2 -> K1 = [H1|K], safe([H2|T],K1)
  ; safe([H2|T],K)
  ).

solution([],[],[]).
solution(State, Config, [Move | Moves]) :-
  action(State, Config, Move, NextState, NextConfig),
  safe(NextConfig,[]),
  solution(NextConfig,NextState, Moves).

solve(File) :-
  open(File, read, Stream),
  read_line_to_string(Stream, Char),
  string_chars(Char,L),
  sum_atoms(L,H),
  length(Moves, _N),
  solution(H,Config,Moves),
  write(Moves), nl.
