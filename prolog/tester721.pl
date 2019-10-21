/* Version October 21, 2019 */

:- [midterm721].

run_tests(Tests, I, F) :-
    run_tests(Tests, 0, 0, I, F).

run_tests([], I, F, I, F) :- !.

run_tests([T|Tests], CI, CF, I, F) :-
    statistics(inferences, I1),
    /* Trick to execute query once without binding its variables. */
    not(not(call(T))),
    !,
    statistics(inferences, I2),
    II is I2 - I1,
    /*
    write(T), nl,
    write('  Success with '), write(II), write(' inferences.'), nl,
    */
    C is CI + II,
    run_tests(Tests, C, CF, I, F).

run_tests([T|Tests], CI, CF, I, F) :-
    write('FAILED: '), write(T), nl,
    CFF is CF + 1,
    run_tests(Tests, CI, CFF, I, F).

test_all :-
    run_tests([ 
	/* first_missing_positive */
	first_missing_positive([99999, 123, 1, 24, 5, 9999999, 222, 3, 4, 7777777, 2], 6),
	(findall(X, first_missing_positive([99, 4, 1, 3, 7, 2], X), L), L = [5]),
	(findall(Y, between(1, 1000, Y), LLL), reverse(LLL, LL), findall(X, first_missing_positive(LL, X), L), L = [1001]),

	/* riffle */
	riffle([1,2,3,4], [5,6,7,8], [1,5,2,6,3,7,4,8], left),
	riffle([1,2,3,4], [5,6,7,8], [5,1,6,2,7,3,8,4], right),
	(riffle([42, bob, 99], [55, jack, tom], [55|_], M), M = right),
	
	/* sz */
	sz(272, 77777777777777770000),
	(findall(S, sz(555, S), L), L = [7770]),
	(findall(S, sz(2727, S), L), L = [777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777]),
	
	/* crag */
	crag([5,4,5], 10),
	(findall(S, crag([3,4,X], S), L), sort(L, [4, 5, 6, 8, 26])),
	(findall((X, Y, Z), crag([X, Y, Z], 26), L), length(L, 12)),

	/* count_dominators */
	count_dominators([], 0),
	count_dominators([33, 22, 11, 64, -2, 5], 2),
	(findall(X, between(1, 1000, X), L), reverse(L, LL), findall(D, count_dominators(LL, D), LD), LD = [1000]),
	
	/* running_median */
	running_median([99, 42, 17, 55, -4, 18, 77], [99, 42, 42, 42, 17, 18, 18]),
	(running_median([42, 42, 42, 42, 42, 42, 42], L), L = [42, 42, 42, 42, 42, 42, 42]),
	running_median([1,2,3,4,5,6], [1,2,2,3,4,5]),

	/* safe_squares_rooks */
	safe_squares_rooks([(2, 2), (3, 1), (5, 5), (2, 5)], 5, 4),
	(findall((X, X), between(1, 50, X), L), safe_squares_rooks(L, 50, S), S = 0),
	safe_squares_rooks([(4,3), (2,2), (1,2)], 10, 56),

	/* trick_winner */
	trick_winner([(five, spades), (queen, diamonds), (ace, spades), (ten, spades)], (ace, spades)),
	(findall(X, trick_winner([(six, spades), (deuce, hearts), (X, spades), (nine, clubs)], (six, spades)), L), length(L, 4)),
	(findall(X, trick_winner([(five, diamonds), X, (ten, hearts), (ten, diamonds)], X), L), length(L, 4)),
	
	/* sum_of_distinct_cubes */
	sum_of_distinct_cubes(777777777, [919, 117, 29, 6]),
	(sum_of_distinct_cubes(123456789, L), L = [497, 88, 22, 8, 7, 6, 5]),
	(X is 10^16+1, sum_of_distinct_cubes(X, L), L = [215443, 4027, 139, 12, 10, 8, 5, 3]),

	/* fibonacci_sum */
	fibonacci_sum(665544332211, [591286729879, 53316291173, 20365011074, 433494437, 102334155, 39088169, 1346269, 28657, 6765, 1597, 34, 2]),
	(X is 10^100, fibonacci_sum(X, L), length(L, 137)),
	(X is 10^1000, fibonacci_sum(X, L), length(L, 1316))
	
    ], I, F),
    write('Executed '), write(I), write(' total inferences. '),
    write('Failed '), write(F), write(' test cases.'), nl.


:- test_all.
