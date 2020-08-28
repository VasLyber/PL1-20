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
    atom_number(A, N),
    read_lines(Stream, N, Chars).

read_lines(Stream, N, Chars) :-
  ( N == 0 -> Chars = []
  ; N > 0  -> read_line_to_string(Stream, Char),
              string_chars(Char, L),
              sum_atoms(L, H),
              reverse(H, O),
              Nm1 is N-1,
              read_lines(Stream, Nm1, RestChars),
              Chars = [O | RestChars]).

replace(_, _, _,_,[], []).
replace(O, R, W, U, [O|T], [R|T2]) :- replace(O, R, W, U, T, T2).
replace(O, R, W, U, [R|T], [O|T2]) :- replace(O, R, W, U, T, T2).
replace(O, R, W, U, [W|T], [U|T2]) :- replace(O, R, W, U, T, T2).
replace(O, R, W, U, [U|T], [W|T2]) :- replace(O, R, W, U, T, T2).

action(situation(State1, []), c, situation(State1, _), Moves):-fail.
action(situation([H|_], [H|_]), c, situation(_, _), _):-fail.
action(situation(State1, [H|T]), c, situation(NewState1, [H|T]), [H1|T1]):-
 (H1==c, fail
 ;H1==r, fail
 ;replace(85,65,71,67, State1, NewState1)).

action(situation([H|T], State2), p, situation(T, [H|State2]), Moves):-safe([H|State2]).
action(situation([H|T], []), p, situation(T, H), Moves).
action(situation([H|T], [H|P]), p, situation(T, [H|P]), Moves).


action(situation(State1, []), r, situation(State1, _), Moves):-fail.
action(situation(State1, [H]), r, situation(State1, _), Moves):-fail.
action(situation([H|_],[H|_]), r, situation(_, _), _):-fail.
action(situation(State1, [H|T]), r, situation(State1, NewState2), [H1|T1]):-
  (H1==r, fail
  ;H1==c, fail
  ;reverse([H|T], NewState2)).

safe([]).
safe([H]).
safe([H, P| T]) :-
      not((H==P)),
      not(memberchk(H, T)),
      safe([P|T]).

empty_set([]).

state_record(State, Parent, Move, [State, Parent, Move]).

remove_from_queue(E, [E|T], T).

empty_queue([]).

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

member_queue(E, S) :- member(E, S).

add_to_set(X, S, S) :- member(X, S), !.
add_to_set(X, S, [X|S]).

member_set(E, S) :- member(E, S).

path(Open, Closed, Goal, Moves) :-
    remove_from_queue(Next_record, Open, _),
    state_record(State, _, _,Next_record),
    State = Goal,
    getmove(Next_record, Closed, Moves).

path(Open, Closed, Goal, Moves2) :-
    remove_from_queue(Next_record, Open, Rest_of_open),
    (bagof(Child, moves(Next_record, Open, Closed, Child) , Children); Children = []),
    add_list_to_queue(Children, Rest_of_open, New_open),
    put_assoc(Next_record, Closed, 15, New_closed),
    path(New_open, New_closed, Goal, Moves2).

moves(State_record, Open, Closed, Child_record) :-
    state_record(State, _, _, State_record),
    action(State, Move, Next, _),
    state_record(Next, _, _, Test),
    not(member_queue(Test, Open)),
    not(member_set(Test, Closed)),
    state_record(Next, State, Move, Child_record).

getmove(State_record, _,[]):-
    state_record(State, nil, nil, State_record).
getmove(State_record, Closed, [Move|Moves]) :-
    state_record(State, Parent, Move, State_record),
    state_record(Parent, _, _, Parent_record),
    member(Parent_record, Closed),
    get_assoc(Parent_record, Closed, 15),
    getmove(Parent_record, Closed, Moves).

add_list_to_queue([], Queue, Queue).
add_list_to_queue([H|T], Queue, New_queue) :-
    add_to_queue(H, Queue, Temp_queue),
    add_list_to_queue(T, Temp_queue, New_queue).

solution_start(Start, Goal, Moves3) :-
  empty_queue(Empty_open),
  state_record(Start, nil, nil, State),
  add_to_queue(State, Empty_open, Open),
  empty_set(Closed),
  path(Open, Closed, Goal, Moves3).

solutions(Ns, Answers,[H|T]) :-
  (
    Ns == 0 -> Answers = []
  ; Ns == 1 -> length(H, I),
               length(Moves, _N),
               (_N>101,_N<I->!
             ; once(solution_start(situation(H,[]), situation([],_), Moves4))),
               Ns1 is Ns-1,
               reverse(Moves4, Moves5),
               string_chars(Str, Moves5),
               solutions(Ns1, RestAnswers, [5,6]),
               Answers = [Str | RestAnswers]
  ; Ns > 1 ->  length(H, I),
               length(Moves, _N),
               (_N>100 ,_N<I-> !
            ;  once(solution_start(situation(H,[]), situation([],_), Moves4))),
               reverse(Moves4, Moves5),
               string_chars(Str, Moves5),
               Ns1 is Ns-1,
               solutions(Ns1, RestAnswers, T),
               Answers = [Str | RestAnswers]).
vaccine(File,Answers) :-
  read_input(File, Ns, Chars),
  time(once(solutions(Ns, Answers, Chars))).
