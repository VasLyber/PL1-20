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

initializecomplementletter(Answer):-
  string_concat('','c',Answer).

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

initialize(Input_queue,Output_queque,String)

initializes(Input_queues, Output_queques, Strings, Ns):-
  (
    Ns == 0 -> Input_queues = [],
               Output_queques = [],
               Strings = []
  ; Ns == 1 -> initialize(Input_queue,Output_queque,String),
               Ns1 is Ns-1,
               initializes(Resinput_queues, Resoutput_queques, RestStrings,[5,6]),
               Input_queues = [Input_queue | Resinput_queues],
               Output_queques = [Output_queque|Resoutput_queques],
               Strings = [String|RestStrings]
  ; Ns > 1 -> initialize(Input_queue,Output_queque,String),
              Ns1 is Ns-1,
              initializes(Ns1, RestAnswers,T),
              Input_queues = [Input_queue | Resinput_queues],
              Output_queques = [Output_queque|Resoutput_queques],
              Strings = [String|RestStrings]
  ).

testlist([]).
testlist([H|T],Answer,AN) :-
  initializes(H,[],Strings,),
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
