read_input(File, N, Numbers) :-
    open(File, read, Stream),
    read_line(Stream, N),
    read_lines(Stream, N, Numbers).

read_lines(Stream, N, Numbers) :-
    ( N == 0 -> Numbers = []
    ; N > 0  -> read_line(Stream, Number),
                Nm1 is N-1,
                read_lines(Stream, Nm1, RestNumbers),
                Numbers = [Number | RestNumbers]).

read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    ( Line = [] -> List = []
    ; atom_codes(A, Line),
      atomic_list_concat(As, ' ', A),
      maplist(atom_number, As, List)
    ).

dec2bin(0,[0]).
dec2bin(1,[1]).
dec2bin(N,L):-
    N > 1,
    X is N mod 2,
    Y is N // 2,
    dec2bin(Y,L1),
    L = [X|L1].

sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
  % predicate works forward: Index,List -> OldElem, Transfer
  nth0(Index,List,OldElem,Transfer),
  % predicate works backwards: Index,NewElem,Transfer -> NewList
  nth0(Index,NewList,NewElem,Transfer).


loop(L,P) :-
  nth0(P,L,K),
  (
    K=<0 ->  plus(P,1,P), loop(L,P)
  ;
    plus(K,-1,U),
    replace_nth0(L, P, Oe,U,L),
    plus(P,-1,P),
    plus(K,2,G),
    replace_nth0(L,P, Oe,G,L)
  ).
convert(E,P):-
  sum_list(E,Q),
  (
    Q>=P ->  portray_clause(E)
    ; loop(E,1),convert(E,P)
  ).




testlist([]).
testlist([H|T]) :-
  dec2bin(H,L),
  sum_list(L,Q),
  length(L,W),
  (
    Q>T -> portray_clause([]),
    fail
  ; Q =:= T ->
    portray_clause(L)
  ; convert(L,T)
  ).
powers2(File) :-
  read_input(File, Ns, Num),
  member(X,Num),
  testlist(X).
