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


action(situation(State1,[]),c,situation(State1,_)):-fail.
action(situation(State1,[H|T]),c,situation(NewState1,[H|T])):-
  replace(85,65,71,67,State1,NewState1).

action(situation([H|T],State2),p,situation(T,[H|State2])):-safe([H|State2]).

action(situation(State1,[]),r,situation(State1,_)):-fail.
action(situation(State1,[H]),r,situation(State1,_)):-fail.
action(situation(State1,[H|T]),r,situation(State1,NewState2)):-
  reverse([H|T],NewState2,[]).

not_yet_seen(State,History) :-                       % will fail if  member(State, History)
   member(State, History),
   !,
   fail.

not_yet_seen(_,_).                                   % the replacement: success if \+ member(State, History)

path(State, State, History,Moves):-!.
path(CurState, FinalState, History,[Move|Moves]) :-
   action(CurState, Move,NextState),                        % generate a safe next state
   not_yet_seen(NextState, History),                 % that hasn't been seen yet
   path(NextState, FinalState, [NextState|History],Moves). % add it to the visited states and recurse

go(StartState, FinalState,Moves) :-
   path(StartState, FinalState, [StartState],Moves).

safe([]).
safe([H]).
safe([H , P | T]) :-
     (H==P),
     safe([P|T]).
safe([H ,P| T]) :-
      not((H==P)),
      not(memberchk(H,T)),
      safe([P|T]).

solution_core(Node,Seen,Goal,DepthLimit,[Move|Moves]) :-
( Node == Goal;
  DepthLimit>0,
  action(Node, Move, Neighbour),not(member(Neighbour,Seen)),
  solution_core(Neighbour,[Neighbour|Seen],Goal,DepthLimit-1,Moves)).

solution_loop(Start,Goal,DepthLimit,Moves):-
( solution_core(Start,[Start],Goal,DepthLimit,Moves);
  solution_loop(Start,Goal,DepthLimit+1,Moves)).

solution_start(Start,Goal,Moves):-
  solution_loop(Start,Goal,Moves,1).
solutions(Ns, Answers,[H|T]) :-
  (
    Ns == 0 -> Answers = []
  ; Ns == 1 -> length(Moves, _N),
               once(go(situation(H,[]),situation([],_),Moves)),
               Ns1 is Ns-1,
               string_chars(Str,Moves),
               solutions(Ns1, RestAnswers,[5,6]),
               Answers = [Str | RestAnswers]
  ; Ns > 1 -> length(Moves, _N),
              once(go(situation(H,[]),situation([],_),Moves)),
              string_chars(Str,Moves),
              Ns1 is Ns-1,
              solutions(Ns1, RestAnswers,T),
              Answers = [Str | RestAnswers]).
plin([H|_],[H1|_],Z):-Z is H1-H.
vaccine(File,Answers) :-
  statistics(runtime,A),
  read_input(File, Ns, Chars),
  once(solutions(Ns,Answers,Chars)),
  statistics(runtime,B),
  plin(A,B,C),
  writeln(C).
