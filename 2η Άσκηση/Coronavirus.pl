go(Start, Goal) :-
	empty_stack(Empty_been_list),
	stack(Start, Empty_been_list, Been_list),
	path(Start, Goal, Been_list).

	% path implements a depth first search in PROLOG

	% Current state = goal, print out been list
path(Goal, Goal, Been_list) :-
	reverse_print_stack(Been_list).

path(State, Goal, Been_list) :-
	mov(State, Next),
	% not(unsafe(Next)),
	not(member_stack(Next, Been_list)),
	stack(Next, Been_list, New_been_list),
	path(Next, Goal, New_been_list), !.

reverse_print_stack(S) :-
	empty_stack(S).
reverse_print_stack(S) :-
	stack(E, Rest, S),
	reverse_print_stack(Rest),
	write(E), nl.

dfs_search_node(Tree,Node, N) :-
	dfs_search_node_(Tree,Node, 1, _, N).


dfs_search_node_(tree(Node, _, _), Node, FN, _, FN) :- !.

dfs_search_node_(tree(_, L, R), Node, CN, _TN, FN) :-
	CN1 is CN + 1,
	dfs_search_node_(L, Node, CN1, TN1, FN),
	(   CN1 \== FN
	->  dfs_search_node_(R, Node, TN1, _TN, FN)
	;   true).

dfs_search_node_(nil, _Node, CN, TN, _FN) :-
	TN is CN + 1.
