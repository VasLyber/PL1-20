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

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.

lead([],[]).
lead([H|T],[H|T]) :-
    dif(H,0).
lead([0|T],T2) :-
    lead(T,T2).

change(0,L,P,K,NewL) :-
  plus(P,1,W),
  loop(L,W,NewL).
change(1,L,P,K,NewL) :-
  plus(K,-1,U),
  replace(L,P,U,O),
  plus(P,-1,W),
  nth0(W,L,PO),
  plus(PO,2,G),
  replace(O,W,G,NewL).

loop(L,P,NewL) :-
  nth0(P,L,K),
  (K>0 -> ISONE is 1
;ISONE is 0 ),
  change(ISONE,L,P,K,NewL).

loopconv(1,L,T,Answer) :-
  loop(L,1,NewL),
  convert(NewL,T,Answer).

loopconv(0,L,T,Answer) :-
  reverse(L,L1),
  lead(L1,L2),
  reverse(L2,L3),
  Answer = L3.

convert(L,T,Answer):-
  sum_list(L,Q),
  (Q<T -> ISONE is 1
;ISONE is 0 ),
  loopconv(ISONE,L,T,Answer).

testlist([]).
testlist([H|T],Answer) :-
  dec2bin(H,L),
  nth0(0,T,Dsrbin),
  sum_list(L,Q),
  (
    Q>Dsrbin -> Answer = []
  ; Q =:= Dsrbin -> Answer = L
  ; convert(L,Dsrbin,Answer)
  ).

testlists(Ns, Answers,[H|T]) :-
  (
    Ns == 0 -> Answers = []
  ; Ns == 1 -> testlist(H,Answer),
               Ns1 is Ns-1,
               testlists(Ns1, RestAnswers,[5,6]),
               Answers = [Answer | RestAnswers]
  ; Ns > 1 -> testlist(H,Answer),
              Ns1 is Ns-1,
              testlists(Ns1, RestAnswers,T),
              Answers = [Answer | RestAnswers]).

powers2(File,Answers) :-
  read_input(File, Ns, Num),
  testlists(Ns, Answers,Num).
