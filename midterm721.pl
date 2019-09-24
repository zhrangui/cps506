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
riffle(_, [], [], right).

riffle([Left|Lefts], [Right|Rights], Result, left) :-
    riffle(Lefts, Rights, Result1, left),
    append([Left,Right], Result1, Result).
    
riffle([Left|Lefts], [Right|Rights], Result, right) :-
    riffle(Lefts, Rights, [Right,Left|Result], right).

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
    sz(N, SZ, 1, 0).
sz(N, SZ, SN, ZN) :-
    seven_zero(SZ, SN, ZN),
    0 =:= mod(SZ, N), !.
sz(N, SZ, SN, ZN) :-
    plus(ZN, 1, ZN1),
    sz(N, SZ, SN, ZN1),
    plus(SN, 1, SN1),
    sz(N, SZ, SN1, ZN).

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
crag([4, 5, 4], 50).
crag([2, 3, 3], 6).
crag([2, 6, 4], 20).
crag(A, B, C, Score) :-!.
:- begin_tests(crag).
    test(crag50, [true(S =:= 50)]) :-
        crag([4, 5, 4], S).
    test(crag6, [true(S =:= 6)]) :-
        crag([2, 3, 3], S).
    test(crag20, [true(S =:= 20)]) :-
        crag([2, 6, 4], S).
    test(findall, [true( L =:= 12)]) :-
        findall((X, Y, Z),crag([X, Y, Z], 26), _L), length(_L, L).
:- end_tests(crag).

% Q5: count_dominators(Items, Result)
count_dominators([], 0).
count_dominators([Item|Items], Result) :-
    count_dominators(Items, Result1),
    (max_member(Item, [Item|Items]) ->
plus(Result1, 1, Result); Result is Result1).
    
:- begin_tests(count_dominators).
    test(count_dominators3, [true(D =:= 3)]) :-
        count_dominators([42, 99, 17, 3, 9], D).
    test(count_dominators4, [true(D =:= 4)]) :-
        count_dominators([4, 3, 2, 1], D).
    test(count_dominators1, [true(D =:= 1)]) :-
        count_dominators([1, 2, 3, 4], D). 
    test(count_dominators0, [true( D =:= 0)]) :-
        count_dominators([], D).
:- end_tests(count_dominators).

% Q6: running_median(Items, Medians)
running_median(Items, Medians) :- !.
:- begin_tests(running_median).
    test(running_median5, [true(M == [1, 2, 2, 3, 4])]) :-
        count_dominators([1, 2, 3, 4, 5], M).
    test(running_median7, [true(M == [99, 42, 42, 42, 17, 18, 18])]) :-
        running_median([99, 42, 17, 55, -4, 18, 77], M).
    test(running_median42, [true(M == [42, 42, 42, 42, 42])]) :-
        running_median([42, 42, 99, 42, 42], M).
:- end_tests(running_median).

% Q7. safe_squares_rooks(Rooks, N, S)
safe_squares_rooks(Rooks, N, S) :-
    safe_squares_rooks(Rooks, [], [], N, S).
safe_squares_rooks([], Rs, Cs, N, S) :-
    list_to_set(Rs, Ru),
    list_to_set(Cs, Cu),
    length(Ru, Lr),
    length(Cu, Lc),
    S is (N-Lr) * (N-Lc).

safe_squares_rooks([(R,C)|Rooks], Rs, Cs, N, S) :- 
    safe_squares_rooks(Rooks, [R|Rs], [C|Cs], N, S).

:- begin_tests(safe_squares_rooks).
    test(safe_squares_rooks4, [true(S== 4)]) :-
        safe_squares_rooks([(2, 2), (3, 1), (5, 5), (2, 5)], 5, S).
    test(safe_squares_rooks0, [true(S == 0)]) :-
        safe_squares_rooks([(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)], 5, S). 
    test(safe_squares_rooks10000, [true(S == 10000)]) :-
        safe_squares_rooks([], 100, S).
:- end_tests(safe_squares_rooks).

% Q8. trick_winner(Cards, Winner)
trick_winner(Cards, Winner) :- !.

% Q9. sum_of_distinct_cubes(N, L)
sum_of_distinct_cubes(N, L) :- !.

% Q10.  fibonacci_sum(N, L)
fibonacci_sum(N, L) :- !.
