
writelist([]).
writelist([H|T]):-
        write(H), writelist(T).

empty_stack([]).
stack(Top, Stack, [Top|Stack]).
member_stack(Element, Stack):-
        member(Element, Stack).


reverse_print_stack(S):-
                empty_stack(S).
reverse_print_stack(S):-
                stack(E, Rest, S),
                reverse_print_stack(Rest),
                write(E), nl.


unsafe(state(X,Y,Y,C)):-
        opp(X, Y).
unsafe(state(X,W,Y,Y)):-
        opp(X, Y).

move(state(X,X,G,C), state(Y,Y,G,C)):-
        opp(X,Y), not(unsafe(state(Y,Y,G,C))),
        writelist(['try farmer takes wolf ',Y,Y,G,C]),nl.

move(state(X,W,X,C), state(Y,W,Y,C)):-
        opp(X,Y), not(unsafe(state(Y,W,Y,C))),
        writelist(['try farmer takes goat ',Y,W,Y,C]),nl.

move(state(X,W,G,X), state(Y,W,G,Y)):-
        opp(X,Y), not(unsafe(state(Y,W,G,Y))),
        writelist(['try farmer takes cabbage ',Y,W,G,Y]),nl.

move(state(X,W,G,C), state(Y,W,G,C)):-
        opp(X,Y), not(unsafe(state(Y,W,G,C))),
        writelist(['try farmer takes self ',Y,W,G,C]),nl.

move(state(F,W,G,C), state(F,W,G,C)):-
        writelist([' BACKTRACK from: ',F,W,G,C]),nl,fail.

path(Goal, Goal, Been_stack):-
        nl, write('Solution Path is: '), nl,
        reverse_print_stack(Been_stack).


path(State, Goal, Been_stack):-
        move(State, Next_state),
        not(member_stack(Next_state, Been_stack)),
        stack(Next_state, Been_stack, New_been_stack),
        path(Next_state, Goal, New_been_stack),!.


opp(e,w).
opp(w,e).


go(Start, Goal):-
        empty_stack(Empty_been_stack),
        stack(Start, Empty_been_stack, Been_stack),
        path(Start, Goal, Been_stack).


test:-go(state(w,w,w,w), state(e,e,e,e)).
