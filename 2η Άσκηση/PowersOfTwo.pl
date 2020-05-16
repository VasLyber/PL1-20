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

convert(L,M) :-
  between(1,,B)


testlist([]).
testlist([H|T]) :-
  dec2bin(H,L),
  sum_list(L,Q),
  length(L,W),
  portray_clause(Q),
  (
    Q>T -> portray_clause([]),
    fail
  ; Q =:= T ->
    portray_clause(L)
%  ; convert(L,M), portray_clause(M)
  ).
powers2(File) :-
  read_input(File, Ns, Num),
  member(X,Num),
  testlist(X).
