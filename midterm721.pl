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
suit(S) :- !.
higher_rank(R1, R2).
trick_winner(Cards, Winner) :- !.
:- begin_tests(trick_winner).
    test(trick_winner9, [true(C == (nine, spades))]) :-
        trick_winner([(four, spades), (deuce, hearts), (nine, spades), (nine, clubs)], C).
    test(trick_winner6, [true(X == five)]) :-
        trick_winner([(six, spades), (deuce, hearts), (X, spades), (nine, clubs)], (six, spades)).
    test(trick_winner, [true(_L = [_G1415, _G1412, _G1409, _G1406, _G1403, _G1400, _G1397, _G1394, _G1391|...])]) :- % , L = 300
        findall(H, trick_winner([C1,C2,C3,C4], (five, spades)), _L), length(_L, L).
    test(trick_winner, [true(LL = 1344)]) :-
        findall((R1,R2,R3,R4), trick_winner([(R1,spades),(R2,spades),(R3,spades),(R4,spades)], (ten, spades)), _L), length(_L, LL).
:- end_tests(trick_winner).

% Q9. sum_of_distinct_cubes(N, L)
sum_of_distinct_cubes(N, _, [N]) :-
    N>0,
    floor(N^(1/3))^3 =:= N.
sum_of_distinct_cubes(N, M, L) :-
    N > 0,
    Root is floor(N^(1/3))-M,
    Root>0,
    Rem is N-Root^3,
    (sum_of_distinct_cubes(Rem, M, L1), 
    [H|_]=L1, H =\= Root ->
    append([Root], L1, L);
    plus(M, 1, M1),
    sum_of_distinct_cubes(N, M1, L)).
sum_of_distinct_cubes(N, L) :-
    sum_of_distinct_cubes(N, 0, L).

:- begin_tests(sum_of_distinct_cubes).
    test(sum_of_distinct_cubes100, [true(L == [4, 3, 2, 1])]) :-
        sum_of_distinct_cubes(100, L).
    test(sum_of_distinct_cubes721, [fail]) :-
        sum_of_distinct_cubes(721, _L). 
    test(sum_of_distinct_cubes12345, [true(L == [20, 12, 10, 9, 8, 6, 5, 3, 2])]) :-
        sum_of_distinct_cubes(12345, L).
    test(sum_of_distinct_cubes999999999999, [true(L == [9999, 669, 81, 27, 7, 6, 2])]) :-
        sum_of_distinct_cubes(999999999999, L).
    test(sum_of_distinct_cubes587848752, [true(L == [837, 113, 30, 13, 6, 5, 4])]) :-
        X is 123^3 + 456^3 + 789^3,
        sum_of_distinct_cubes(X, L).
:- end_tests(sum_of_distinct_cubes).

% Q10. fibonacci_sum(N, L)

fibs_upto(_, [1, 1]).
fibs_upto(N, L) :-
    [Lf,Ls|_]=L,
    F is Lf+Ls,
    N < F,!.
fibs_upto(N, L) :-
    [Lf,Ls|_]=L,
    F is Lf+Ls,
    fibs_upto(N, [F|L]).   

fibonacci_sum(N, L) :- !.
:- begin_tests(fibonacci_sum).
    test(fibs_upto, [true(L == [34, 21, 13, 8, 5, 3, 2, 1, 1])]) :-
        fibs_upto(34, L).
    test(fibs_upto77, [true(L == [55, 34, 21, 13, 8, 5, 3, 2, 1, 1])]) :-
        fibs_upto(77, L).    
    test(fibonacci_sum30, [true(L == [21, 8, 1])]) :-
        fibonacci_sum(30, L).
    test(fibonacci_sum1000000, [true(L == [832040, 121393, 46368, 144, 55])]) :-
        fibonacci_sum(1000000, L).
    test(fibonacci_sum100000000000000000000, [true(L == [83621143489848422977, 12200160415121876738, 2880067194370816120, 1100087778366101931, 160500643816367088, 37889062373143906, 117669030460994, 27777890035288, 4052739537881|...])]) :-
        X is 10^20,
        fibonacci_sum(X, L). 
:- end_tests(fibonacci_sum).
