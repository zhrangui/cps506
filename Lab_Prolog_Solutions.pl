my_same_length([], []).
my_same_length([_|T1], [_|T2]) :-
    my_same_length(T1, T2).

/* Linear stack version without tail calls. */

my_length([], 0).
my_length([_|T], N) :-
    my_length(T, N2),
    plus(N2, 1, N).

/* O(1) space version with accumulators and tail calls. */

my_length_acc(L, N) :-
    my_length_acc(L, 0, N).
my_length_acc([], N, N).
my_length_acc([_|T], Acc, N) :-
    plus(Acc, 1, Acc2),
    my_length_acc(T, Acc2, N).

my_prefix([], L) :-
    is_list(L).
my_prefix([H|L1], [H|L2]) :-
   my_prefix(L1, L2).

my_memberchk(X, [X|_]) :-
    !.
my_memberchk(X, [_|L]) :-
    my_memberchk(X, L).

/* Use the metapredicate var(X) to make my_nth1 reversible. */

my_nth1(1, [X|_], X).
my_nth1(N, [_|T], X) :-
    var(N), !,
    my_nth1(N2, T, X),
    N is N2 + 1.
my_nth1(N, [_|T], X) :-
    N > 1,
    N2 is N - 1,
    my_nth1(N2, T, X).

my_delete([], _, []).
my_delete([X|T], X, T2) :-
    !,
    my_delete(T, X, T2).
my_delete([H|T], X, [H|T2]) :-
    my_delete(T, X, T2).

/* Like the SWI-Prolog sublist predicate, my_sublist repeats solutions. */

my_sublist([], _).
my_sublist([X|L1], [X|L2]) :-
    my_sublist(L1, L2).
my_sublist(L1, [_|L2]) :-
    my_sublist(L1, L2).

/* The metapredicate var(X) is again used to make my_take reversible. See
 * what happens if we move the base case formula before the other two. */

my_take(N, [X|L1], [X|L2]) :-
    nonvar(N),
    N > 0,
    !,
    N2 is N - 1,
    my_take(N2, L1, L2).
my_take(N, [X|L1], [X|L2]) :-
    var(N),
    my_take(N2, L1, L2),
    N is N2 + 1.
my_take(0, _, []).

/* Ditto. */

my_drop(N, [_|L], L2) :-
    nonvar(N),
    N > 0,
    !,
    N2 is N - 1,
    my_drop(N2, L, L2).
my_drop(N, [_|L], L2) :-
    var(N),
    my_drop(N2, L, L2),
    N is N2 + 1.
my_drop(0, L, L).

/* Same as with my_sublist, this predicate produces repeated solutions. */

my_bounded_sublist([], _, _).
my_bounded_sublist([X|L], [X|L2], N) :-
    N > 0,
    N2 is N - 1,
    my_bounded_sublist(L, L2, N2).
my_bounded_sublist(L, [_|L2], N) :-
    my_bounded_sublist(L, L2, N).

my_memberchk_eq(X, [Y|_]) :-
    X == Y,
    !.
my_memberchk_eq(X, [_|L]) :-
    my_memberchk_eq(X, L).

my_between(N, N, N) :- !.
my_between(M, N, M) :- M < N.
my_between(M, N, _) :- M > N, !, fail.
my_between(M, N, I) :-
    M2 is M + 1,
    my_between(M2, N, I).

/* While-loops can be simulated with mutual recursion. */

squares :-
    read(X),
    process_square(X).

process_square(stop) :-
    !. /* base case of mutual recursion, no further calls */
process_square(X) :-
    integer(X),
    !,
    X2 is X * X,
    write(X2), nl,
    squares. /* tail call, so no problemo for the stack size */
process_square(_) :-
    write('Please enter an integer.'), nl,
    squares. /* ditto */

/* Again, use an accumulator to avoid stack overflow from deep recursions. */

collatz(N, S) :- collatz(N, S, 0).
collatz(1, S, S) :-
    !.
collatz(N, S, Acc) :-
    N > 1,
    0 =:= N mod 2,
    !,
    N2 is div(N, 2), /* integer division */
    Acc2 is Acc + 1,
    collatz(N2, S, Acc2).
collatz(N, S, Acc) :-
    N > 1,
    1 =:= N mod 2,
    !,
    N2 is 3 * N + 1,
    Acc2 is Acc + 1,
    collatz(N2, S, Acc2).

zip(L1, L2, L) :-
    zip1(L1, L2, L).

zip1([], L, L).
zip1([X|L1], L2, [X|L3]) :-
    zip2(L1, L2, L3).

zip2(L, [], L).
zip2(L1, [X|L2], [X|L3]) :-
    zip1(L1, L2, L3).

accum(L, A) :-
    accum(L, A, 0).

accum([], [], _).
accum([X|T], [X2|A], Acc) :-
    plus(X, Acc, X2),
    accum(T, A, X2).

subset_sum(_, [], 0) :- !.
subset_sum([_|T], L, G) :- 
    subset_sum(T, L, G).
subset_sum([H|T], [H|L], G) :-
    plus(G2, H, G),
    subset_sum(T, L, G2).

