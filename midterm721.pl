% Q1: first_missing_positive(Items, Result)
first_missing_positive(Items, Result) :-
    first_missing_positive(Items, 1, Result).
first_missing_positive(Items, Result, Result) :-
    \+ member(Result, Items).
first_missing_positive(Items, N, Result) :-
    N1 is N+1,
    first_missing_positive(Result, N1, Items).

:- begin_tests(first_missing_positive).
test(first_missing_positive, [true(N =:= 3)]) :-
    first_missing_positive([6, 8, 2, 999, 1, 4, 7], N).
test(first_missing_positive_minus, [true(N =:= 1)]) :-
    first_missing_positive([42, 99, 123456, -3, 777], N).
test(first_missing_positive_str, [true(N =:= 1)]) :-
    first_missing_positive([bob, jack, foo(bar, baz, qux)], N).
:- end_tests(first_missing_positive).

% Q2: riffle(Left, Right, Result, Mode)
riffle([], _, [], left).
riffle(_, [], [], left).
riffle([], _, [], right).
riffle(_, [], [], right).
riffle([], _, R, _, R).
riffle(_,  _, R, _, R).
riffle([Left|Lefts], [Right|Rights], Result, left, R) :-
    riffle(Lefts, Rights, [Left,Right|Result], left, R).
    
riffle([Left|Lefts], [Right|Rights], Result, right, R) :-
    riffle(Lefts, Rights, [Right,Left|Result], right, R).

:- begin_tests(riffle).
test(riffle, [true(L == [bob, 99, 42, hello, foo(bar), world])]) :-
    riffle([bob, 42, foo(bar)], [99, hello, world], L, left).
test(rifle_false, [fail]) :-
    riffle(_, _, [odd, number, of, elements, cannot, succeed, here], _).
test(rifle_mode, [true(Mode == false)]) :-
    riffle([42, bob, 99], [55, jack, tom], [55|_], Mode).
:- end_tests(riffle).

% Q3: sz(N, SZ)
seven_zero_rec(0, 0, 0) :- !.
seven_zero_rec(N, S, Z) :-
    Z > 0,
    plus(Z, -1, Z1),
    seven_zero_rec(N1, S, Z1),
    N is N1*10.
seven_zero_rec(N, S, 0) :-
    S > 0,
    plus(S, -1, S1),
    seven_zero_rec(N1, S1, 0),
    N is N1*10+7.

seven_zero(N, S, Z) :-
    seven_zero(N, S, Z, 0).
seven_zero(N, 0, 0, N).
seven_zero(N, S, Z, Acc) :-
    S > 0,
    plus(S, -1, S1),
    Acc1 is Acc*10+7,
    seven_zero(N, S1, Z, Acc1).
seven_zero(N, 0, Z, Acc) :-
    Z > 0,
    plus(Z, -1, Z1),
    Acc1 is Acc*10,
    seven_zero(N, 0, Z1, Acc1).

sz(N, SZ) :-
    sz(N, 7, SZ).
sz(N, SZ, SZ) :-
    0 =:= mod(SZ, N).
sz(N, R, SZ) :-
    R0 is R*10,
    R7 is R*10 + 7,
    sz(N, R0, SZ);
    sz(N, R7, SZ).

:- begin_tests(sz).
    test(sz42, [true(SZ =:= 7770)]) :-
        sz(42, SZ).
    test(sz400000, [true(SZ =:= 70000000)]) :-
        sz(400000, SZ).
    test(sz123, [true(SZ =:= 777777777777777)]) :-
        sz(123, SZ).
    test(sz721, [true(SZ =:= 7777777777777777777777777777777777)]) :-
        sz(721, SZ).
    test(sz1234, [true(SZ =:= 77777777777777777777777777777777777777777777777777777777777777777777777777777777777777770)]) :-
        sz(1234, SZ).
:- end_tests(sz).

% Q4: crag(A, B, C, Score)
crag(A, B, C, Score) :- !.

% Q5: count_dominators(Items, Result)
count_dominators(Items, Result) :- !.

% Q6: running_median(Items, Medians)
running_median(Items, Medians) :- !.

% Q7. safe_squares_rooks(Rooks, N, S)
safe_squares_rooks(Rooks, N, S) :- !.

% Q8. trick_winner(Cards, Winner)
trick_winner(Cards, Winner) :- !.

% Q9. sum_of_distinct_cubes(N, L)
sum_of_distinct_cubes(N, L) :- !.

% Q10.  fibonacci_sum(N, L)
fibonacci_sum(N, L) :- !.
