first_missing_positive(Items, Result):-
    first_missing_positive(Items, 1, Result).
first_missing_positive(Items, Result, Result):-
    \+ member(Result, Items).
first_missing_positive(Items, N, Result):-
    N1 is N+1,
    first_missing_positive(Result, N1, Items).

riffle([], _, Result, left):-
    Result = Result.
riffle([Left|Lefts], [Right|Rights], Result, left):-
    riffle(Lefts, Rights, [Left,Right|Result], left).
riffle([Left|Lefts], [Right|Rights], Result, right):-
    riffle(Lefts, Rights, [Right,Left|Result], right).

% sz(N, SZ).
% crag(A, B, C, Score).
% count_dominators(Items, Result).
% running_median(Items, Medians).
% safe_squares_rooks(Rooks, N, S).
% trick_winner(Cards, Winner).
% sum_of_distinct_cubes(N, L).
% fibonacci_sum(N, L).

:- begin_tests(first_missing_positive).
test(first_missing_positive, [true(N =:= 3)]):-
    first_missing_positive([6, 8, 2, 999, 1, 4, 7], N).
test(first_missing_positive1, [true(N =:= 1)]):-
    first_missing_positive([42, 99, 123456, -3, 777], N).
test(first_missing_positive2, [true(N =:= 1)]):-
    first_missing_positive([bob, jack, foo(bar, baz, qux)], N).
:- end_tests(first_missing_positive).

:- begin_tests(riffle).
test(riffle, [true(L == [bob, 99, 42, hello, foo(bar), world])]) :-
    riffle([bob, 42, foo(bar)], [99, hello, world], L, left).
:- end_tests(riffle).
