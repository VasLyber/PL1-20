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
              Nm1 is N-1,
              read_lines(Stream, Nm1, RestChars),
              Chars = [L | RestChars]).

vaccine(File,Answers) :-
  read_input(File, Ns, Answers).
