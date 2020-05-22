read_input(File, N, Numbers) :-
  %  open(File, read, Stream),
  %  read_line(Stream, N),
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

coronograph(File) :-
	open(File, read, Stream),
	read_line(Stream, N),
	read_input(File, Ns, Num),
