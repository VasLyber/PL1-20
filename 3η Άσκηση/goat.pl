start([a,a,a,a]).
goal([b,b,b,b]).

transition0([G,C,W,F1],[G,C,W,F2]).
transition0([F1,C,W,F1],[F2,C,W,F2]).
transition0([G,F1,W,F1],[G,F2,W,F2]).
transition0([G,C,F1,F1],[G,C,F2,F2]).

transition(X,Y) :- (transition0(X,Y);transition0(Y,X)), valid(X),valid(Y).

valid([F,_,_,F]).
valid([G,C,W,_]) :- G \== C, G \==W.

% Iterative Depth-Search Algorithm
ids_core(Node, Path, Goal, DepthLimit, ReturnPath) :-
    (Node == Goal,reverse(Path,ReturnPath);
    transition0(Node,Neighbour),not(member(Neighbour,Path)),
    ids_core(Neighbour,[Node|Path],Goal,DepthLimit-1,ReturnPath)).

ids_loop(Start,Goal,DepthLimit,ReturnPath) :-
  (  ids_core(Start,[Start],Goal,DepthLimit,ReturnPath);
    ids_loop(Start,Goal,DepthLimit+1,ReturnPath)).

ids_start(Start,Goal,ReturnPath) :- ids_loop(Start,Goal,ReturnPath,1).

save_the_goat() :-
    start(S),
    goal(G),
    ids_start(S,G,R),
    write(R).
