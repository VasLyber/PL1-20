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

replace(_, _, _,_,[], []).
replace(O, R, W, U, [O|T], [R|T2]) :- replace(O, R, W, U, T, T2).
replace(O, R, W, U, [R|T], [O|T2]) :- replace(O, R, W, U, T, T2).
replace(O, R, W, U, [W|T], [U|T2]) :- replace(O, R, W, U, T, T2).
replace(O, R, W, U, [U|T], [W|T2]) :- replace(O, R, W, U, T, T2).


action(situation(State1,[]),c,situation(State1,NewState2)):-fail.
action(situation(State1,[H|T]),c,situation(NewState1,[H|T])):-
  replace(85,65,71,67,State1,NewState1).

action(situation([H|T],State2),p,situation(T,[H|State2])):-safe([H|State2]).
action(situation(State1,[]),r,situation(State1,NewState2)):-fail.
action(situation(State1,[H]),r,situation(State1,NewState2)):-fail.
action(situation(State1,[H|T]),r,situation(State1,NewState2)):-
  reverse([H|T],NewState2,[]).

safe([]).
safe([H]).
safe([H , P | T]) :-
     (H==P),
     safe([P|T]).
safe([H ,P| T]) :-
      not((H==P)),
      not(memberchk(H,T)),
      safe([P|T]).

solution(situation([],NewState2),[],_).
solution(situation(State1,State2), [Move | Moves],Seen) :-
  action(situation(State1,State2), Move, situation(NewState1, NewState2)),not(member(situation(NewState1,NewState2),Seen)),
  solution(situation(NewState1,NewState2), Moves,[situation(NewState1,NewState2)|Seen]).

solutions(Ns, Answers,[H|T]) :-
  (
    Ns == 0 -> Answers = []
  ; Ns == 1 -> length(Moves, _N),
               once(solution(situation(H,[]),Moves,[])),
               Ns1 is Ns-1,
               string_chars(Str,Moves),
               solutions(Ns1, RestAnswers,[5,6]),
               Answers = [Str | RestAnswers]
  ; Ns > 1 -> length(Moves, _N),
              once(solution(situation(H,[]),Moves,[])),
              string_chars(Str,Moves),
              Ns1 is Ns-1,
              solutions(Ns1, RestAnswers,T),
              Answers = [Str | RestAnswers]).
plin([H|T],[H1|T1],Z):-Z is H1-H.
vaccine(File,Answers) :-
  statistics(runtime,A),
  read_input(File, Ns, Chars),
  once(solutions(Ns,Answers,Chars)),
  statistics(runtime,B),
  plin(A,B,C),
  writeln(C).
