read_input(Stream, Term, K) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, [KA|LA]),
    L is LA,
    K is KA,
    functor(Term, array, K),
    read_lines(Stream, L, Term).

read_lines(Stream, N, Term) :-
  ( N == 0 ->   true
  ; N > 0  ->   read_line_to_codes(Stream, Line),
                atom_codes(A, Line),
                atomic_list_concat(As, ' ', A),
                maplist(atom_number, As, [KA|LA]),
                L is LA,
                K is KA,
                arg(K,Term,LO),
                ( not(var(LO)) -> append([L],LO,LI),
                                  setarg(K, Term, LI)
                ; var(LO)      -> setarg(K, Term, [L])
                ),
                arg(L,Term,KO),
                ( not(var(KO)) -> append([K],KO,KI),
                                  setarg(L, Term, KI)
                ; var(KO)      -> setarg(L, Term, [K])
                ),
                Nm1 is N-1,
                read_lines(Stream, Nm1,Term)
  ).


initialize(Visited,N) :-
( N == 0 ->   true
; N > 0  -> setarg(N,Visited,0),
            Nm1 is N-1,
            initialize(Visited,Nm1)
).

iteratedfs([],Term,Visited).
iteratedfs([H|T],Term,Visited) :-
arg(H,Visited,TM),
( TM == 0 -> dfsutil(H,Visited,Term),
             iteratedfs(T,Term,Visited)
; TM == 1 -> iteratedfs(T,Term,Visited)
).

dfsutil(K,Visited,Term) :-
  setarg(K,Visited,1),
  arg(K,Term,List),
  iteratedfs(List,Term,Visited).

dfs(K,Term,Visited,L) :-
  functor(Visited,array,K),
  initialize(Visited,K),
  dfsutil(L,Visited,Term).

initializecycle(Visited,N,Parent) :-
( N == 0 ->   true
; N > 0  -> setarg(N,Visited,0),
            setarg(N,Parent,-1),
            Nm1 is N-1,
            initializecycle(Visited,Nm1,Parent)
).
returncycle(Parent,Cycle,H,P,PA) :-
  (PA \= P ->  arg(P,Parent,Q),
              returncycle(Parent,ResCycle,H,Q,PA),
              Cycle = [P|ResCycle]
  ;PA == P -> Cycle = [H,PA]
  ).

iteratecycle([],Term,Visited,P,Parent,Cycle).
iteratecycle([H|T],Term,Visited,P,Parent,Cycle) :-
arg(H,Visited,TM),
arg(P,Parent,PA),
( TM == 0 -> iscycleutil(H,Visited,Term,Parent,P,Cycle),
            (not(var(Cycle))-> !
            ; var(Cycle)->iteratecycle(T,Term,Visited,P,Parent,Cycle)
            )
; TM == 1 , PA\=H -> returncycle(Parent,Cycle,P,PA,H)
; TM == 1 -> iteratecycle(T,Term,Visited,P,Parent,Cycle)
).

iscycleutil(K,Visited,Term,Parent,P,Cycle) :-
  setarg(K,Visited,1),
  arg(K,Term,List),
  setarg(K,Parent,P),
  iteratecycle(List,Term,Visited,K,Parent,Cycle).

remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result).
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).

iscycle(K,Term,Visited,Cycle) :-
  functor(Visited,array,K),
  functor(Parent,array,K),
  initializecycle(Visited,K,Parent),
  iscycleutil(K,Visited,Term,Parent,-1,Cycle).

newgraph(Term,[],Cycle).
newgraph(Term,[H|T],Cycle):-
  arg(H,Term,L),
  remove_list(L,Cycle,I),
  setarg(H,Term,I),
  newgraph(Term,T,Cycle)
  .

loop(Visited,Ass,N):-
  (N == 0 -> true
  ;N > 0 -> arg(N,Visited,U),
            arg(1,Ass,Y),
            (U ==1 -> plus(Y,1,R)
            ;U\=1 -> R is Y
            ),
            setarg(1,Ass,R),
            Nm is N - 1,
            loop(Visited,Ass,Nm)
  ).
fortree(Term,[],NUM,K,O) :-
  NUM = [].
fortree(Term,[H|T],NUM,K,O):-
  dfs(K,Term,Visited,H),
  functor(Ass,array,1),
  setarg(1,Ass,0),
  loop(Visited,Ass,K),
  arg(1,Ass,W),
  fortree(Term,T,ResNUM,K,O),
  NUM = [W|ResNUM].

final(Sum,Final,Fin):-
  Fin = [Sum,Final].

coronograph(File) :-
  open(File, read, Stream),
  read_input(File,Term,K),
  dfs(K,Term,Visited,K),
  Visited =..List,
  min_member(Min, List),
  (Min == 0 -> portray_clause("NO CORONA"),!
   ;Min\=0 ->iscycle(K,Term,Visited,Cycle),
            (var(Cycle) -> portray_clause("NO CORONA"),!
                            ;not(var(Cycle))->newgraph(Term,Cycle,Cycle),
                                              iscycle(K,Term,Visited,Cycle1),
                                              (not(var(Cycle1)) -> portray_clause("NO CORONA"),!
                                              ;var(Cycle1)-> length(Cycle,Sum),
                                                                 fortree(Term,Cycle,NUM,K,Sum),
                                                                 msort(NUM, Final),
                                                                 final(Sum,Final,Fin),
                                                                 portray_clause(Fin)

                                              )
            )

  ).
