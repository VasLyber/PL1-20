reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

sum_atoms([], []).
sum_atoms([H|T], [HeadSum|RT]) :-
  atom_codes(H, Codes),
  sum_letters(Codes, HeadSum),
  sum_atoms(T, RT).

sum_letters([], 0).
sum_letters([H|T], Res) :-
  sum_letters(T, TailSum),
  Res is (H) + TailSum.

read_input(File, N, Chars) :-
    open(File, read, Stream),
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atom_number(A,N),
    read_lines(Stream, N, Chars).

read_lines(Stream, N, Chars) :-
  ( N == 0 -> Chars = []
  ; N > 0  -> read_line_to_string(Stream, Char),
              string_chars(Char,L),
              sum_atoms(L,H),
              reverse(H,O,[]),
              Nm1 is N-1,
              read_lines(Stream, Nm1, RestChars),
              Chars = [O | RestChars]).
complement([],Z,Z).
complement([H|T],Z,Acc) :-
  (
    H == 85 ->  complement(T,Z,[65|Acc])
  ; H == 65 ->  complement(T,Z,[85|Acc])
  ; H == 71 ->  complement(T,Z,[67|Acc])
  ; H == 67 ->  complement(T,Z,[71|Acc])
  ).

copy(L,R) :- accCp(L,R).
accCp([],[]).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).

push([H|T],NewState1,State2,NewState2):-
  copy(State2,NState2),
  append([H],NState2,NewState2),
  copy(T,NewState1).

action(State1,State2,c,NewState1,NewState2):-
  copy(State2,NewState2),
  complement(State1,NewState,[]),
  reverse(NewState,NewState1,[]).
action(State1,State2,p,NewState1,NewState2):-
  push(State1,NewState1,State2,NewState2).
action(State1,State2,r,NewState1,NewState2):-
  copy(State1,NewState1),
  reverse(State2,NewState2,[]).

all_diff(L) :- \+ (append(_,[X|R],L), memberchk(X,R)).
remove_duplicates([],[]).
remove_duplicates([H],[H]).
remove_duplicates([H , P | T], List) :-
     (H==P),
     remove_duplicates([P|T], List).

remove_duplicates([H ,P| T], [H|T1]) :-
      \+(H==P),
      remove_duplicates( [P|T], T1).

safe([]).
safe(NewState2):-
    remove_duplicates(NewState2,NS2),
    all_diff(NS2).

solution([],NewState2,[]):-safe(NewState2),!.
solution(State1, State2, [Move | Moves]) :-
  action(State1, State2, Move, NewState1, NewState2),
  safe(NewState2),
  solution(NewState1,NewState2, Moves).

solutions(Ns, Answers,[H|T]) :-
  (
    Ns == 0 -> Answers = []
  ; Ns == 1 -> length(Moves, _N),
               solution(H,[],Moves),!,
               Ns1 is Ns-1,
               string_chars(Str,Moves),
               solutions(Ns1, RestAnswers,[5,6]),
               Answers = [Str | RestAnswers]
  ; Ns > 1 -> length(Moves, _N),
              solution(H,[],Moves),!,
              string_chars(Str,Moves),
              Ns1 is Ns-1,
              solutions(Ns1, RestAnswers,T),
              Answers = [Str | RestAnswers]).
plin([H|T],[H1|T1],Z):-Z is H1-H.
vaccine(File,Answers) :-
  statistics(runtime,A),
  read_input(File, Ns, Chars),
  solutions(Ns,Answers,Chars),
  statistics(runtime,B),
  plin(A,B,C),
  writeln(C).
