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

initializecomplement(Output,Sofar,Answer):-
  string_concat('','c',Sofar).

addcomplementletter(Answer,AN):-
  An = Answer,
  string_concat(Answer,'c',AN).

initializepushletter(Answer):-
  string_concat('','p',Answer).
addpushletter(Answer,AN):-
  An = Answer,
  string_concat(Answer,'p',AN).

initializereverseletter(Answer):-
  string_concat('','r',Answer).
addpushletter(Answer,AN):-
  An = Answer,
  string_concat(Answer,'r',AN).

testlist([]).
testlist([H|T],Answer,AN) :-
  initializecomplementletter(Answer),
  addcomplementletter(Answer,AN).

testlists(Ns, Answers,[H|T]) :-
  (
    Ns == 0 -> Answers = []
  ; Ns == 1 -> testlist(H,Answer,AN),
               Ns1 is Ns-1,
               testlists(Ns1, RestAnswers,[5,6]),
               Answers = [AN | RestAnswers]
  ; Ns > 1 -> testlist(H,Answer,AN),
              Ns1 is Ns-1,
              testlists(Ns1, RestAnswers,T),
              Answers = [AN | RestAnswers]).

vaccine(File,Answers) :-
  read_input(File, Ns, Chars),
  testlists(Ns,Answers,Chars).
