read_input(File, Term) :-
    open(File, read, Stream),
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
coronograph(File) :-
  read_input(File,Term),
  portray_clause(Term) .
