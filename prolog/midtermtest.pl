/*  CCPS 721 Midterm
    Name: Rui Zhang
    No:   500736315
    command run_tests to run all unit test cases or run_tests(fibonacci_sum) to execute individual test cases
    */
:- [midterm721].

:- begin_tests(first_missing_positive).
test(first_missing_positive_3, [true(N =:= 3)]) :-
    first_missing_positive([6, 8, 2, 999, 1, 4, 7], N).
test(first_missing_positive_1, [true(N =:= 1)]) :-
    first_missing_positive([42, 99, 123456, -3, 777], N).
test(first_missing_positive_str, [true(N =:= 1)]) :-
    first_missing_positive([bob, jack, foo(bar, baz, qux)], N).
test(first_missing_positive_2_1, [true(N =:= 1)]) :-
    first_missing_positive([2, 3, 4, [1, 1, 1, 1]], N).
:- end_tests(first_missing_positive).

:- begin_tests(riffle).
test(riffle, [true(L == [bob, 99, 42, hello, foo(bar), world])]) :-
    riffle([bob, 42, foo(bar)], [99, hello, world], L, left).
test(rifle_false, [fail]) :-
    riffle(_, _, [odd, number, of, elements, cannot, succeed, here], _).
test(rifle_mode, [true(Mode == right)]) :-
    riffle([42, bob, 99], [55, jack, tom], [55|_], Mode).
test(rifle_mode_all, [true([L1, L2, M] == [[A, C, E], [B, D, F], left])]) :-
    riffle(L1, L2, [A, B, C, D, E, F], M).
:- end_tests(riffle).

:- begin_tests(sz).
    test(seven_zero70, [true(N =:= 7777700000000)]) :-
        seven_zero(N, 5, 8).
    test(seven_zero7, [true(N =:= 77777777777777777777)]) :-
        seven_zero(N, 20, 0).
    test(seven_zero0, [true(N =:= 0)]) :-
        seven_zero(N, 0, 10000000000).
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

:- begin_tests(crag).
    test(crag50, [true(S =:= 50)]) :-
        crag([4, 5, 4], S).
    test(crag6, [true(S =:= 6)]) :-
        crag([2, 3, 3], S).
    test(crag20, [true(S =:= 20)]) :-
        crag([2, 6, 4], S).
    test(findall, [true(LL =:= 12)]) :-
        findall((X, Y, Z),crag([X, Y, Z], 26), L), length(L, LL).
:- end_tests(crag).
    
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

:- begin_tests(running_median).
    test(running_median5, [true(M == [1, 2, 2, 3, 4])]) :-
        running_median([1, 2, 3, 4, 5], M).
    test(running_median7, [true(M == [99, 42, 42, 42, 17, 18, 18])]) :-
        running_median([99, 42, 17, 55, -4, 18, 77], M).
    test(running_median42, [true(M == [42, 42, 42, 42, 42])]) :-
        running_median([42, 42, 99, 42, 42], M).
:- end_tests(running_median).

:- begin_tests(safe_squares_rooks).
    test(safe_squares_rooks4, [true(S == 4)]) :-
        safe_squares_rooks([(2, 2), (3, 1), (5, 5), (2, 5)], 5, S).
    test(safe_squares_rooks0, [true(S == 0)]) :-
        safe_squares_rooks([(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)], 5, S). 
    test(safe_squares_rooks10000, [true(S == 10000)]) :-
        safe_squares_rooks([], 100, S).
:- end_tests(safe_squares_rooks).

:- begin_tests(trick_winner).
    test(trick_winner_nine, [true(C == (nine, spades))]) :-
        trick_winner([(four, spades), (deuce, hearts), (nine, spades), (nine, clubs)], C).
    test(trick_winner6, [true(X == five)]) :-
        trick_winner([(six, spades), (deuce, hearts), (X, spades), (nine, clubs)], (six, spades)).
    test(trick_winner_300, [true(LL =:= 375)]) :-
        findall(_, trick_winner([_C1,_C2,_C3,_C4], (five, spades)), L), length(L, LL).
    test(trick_winner_1344, [true(LL = 1344)]) :-
        findall((R1,R2,R3,R4), trick_winner([(R1,spades),(R2,spades),(R3,spades),(R4,spades)], (ten, spades)), L), length(L, LL).
:- end_tests(trick_winner).

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

:- begin_tests(fibonacci_sum).
    test(fibonacci_sum30, [true(L == [21, 8, 1])]) :-
        fibonacci_sum(30, L).
    test(fibonacci_sum1000000, [true(L == [832040, 121393, 46368, 144, 55])]) :-
        fibonacci_sum(1000000, L).
    test(fibonacci_sum100000000000000000000, [true(L == [83621143489848422977, 12200160415121876738, 2880067194370816120, 1100087778366101931,
            160500643816367088, 37889062373143906, 117669030460994, 27777890035288, 4052739537881
            ,1548008755920,365435296162,2971215073,24157817,3524578,196418,75025,10946,4181,610,233,89,21,3,1])]) :-
        X is 10^20,
        fibonacci_sum(X, L).
    test(fibs_upto, [true(L == [34, 21, 13, 8, 5, 3, 2, 1, 1])]) :-
        fibs_upto(34, L).
    test(fibs_upto77, [true(L == [55, 34, 21, 13, 8, 5, 3, 2, 1, 1])]) :-
        fibs_upto(77, L).
    test(fibs_upto10_10, [true(L == [7778742049, 4807526976, 2971215073, 1836311903, 1134903170, 701408733, 433494437, 267914296, 165580141,
        102334155,63245986,39088169,24157817,14930352,9227465,5702887,3524578,2178309,1346269,832040,514229,317811,196418,121393,75025,46368,
        28657,17711,10946,6765,4181,2584,1597,987,610,377,233,144,89,55,34,21,13,8,5,3,2,1,1])]) :-
        fibs_upto(10^10, L).
:- end_tests(fibonacci_sum).
