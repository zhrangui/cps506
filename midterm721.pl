first_missing_positive(Items, Result):-
    R
    \+ member(Result, Items).

%riffle(Left, Right, Result, Mode).
% sz(N, SZ).
% crag(A, B, C, Score).
% count_dominators(Items, Result).
% running_median(Items, Medians).
% safe_squares_rooks(Rooks, N, S).
% trick_winner(Cards, Winner).
% sum_of_distinct_cubes(N, L).
% fibonacci_sum(N, L).

:- begin_tests(midterm721).
test(first_missing_positive) :-
    first_missing_positive([6, 8, 2, 999, 1, 4, 7], N),
    assertion(N == 3). 
:- end_tests(midterm721).
