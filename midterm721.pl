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


riffle([], _, Result, left) :-
    Result = Result.
riffle([Left|Lefts], [Right|Rights], Result, left) :-
    riffle(Lefts, Rights, [Left,Right|Result], left).
riffle([Left|Lefts], [Right|Rights], Result, right) :-
    riffle(Lefts, Rights, [Right,Left|Result], right).

:- begin_tests(riffle).
test(riffle, [true(L == [bob, 99, 42, hello, foo(bar), world])]) :-
    riffle([bob, 42, foo(bar)], [99, hello, world], L, left).
test(rifle_false, [true(false)]) :-
    riffle(L1, L2, [odd, number, of, elements, cannot, succeed, here], M).
test(rifle_mode, [true(Mode == false)]) :-
    riffle([42, bob, 99], [55, jack, tom], [55|_], Mode). 
:- end_tests(riffle).


seven_zero1(_, 0, 0) :- 
    seven_zero1(0, 0, 0).
seven_zero1(N, S, Z) :-
    S > 0,
    plus(S, -1, S1),
    seven_zero1(N, S1, Z),
    N is N*10+7.
seven_zero1(N, S, Z) :-
    Z > 0,
    plus(Z, -1, Z1),
    seven_zero1(N, S, Z1),
    N is N*10.

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


% crag(A, B, C, Score).
% count_dominators(Items, Result).
% running_median(Items, Medians).
% safe_squares_rooks(Rooks, N, S).
% trick_winner(Cards, Winner).
% sum_of_distinct_cubes(N, L).
% fibonacci_sum(N, L).
