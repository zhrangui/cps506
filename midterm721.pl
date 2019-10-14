/*  CCPS 721 Midterm
    Name: Rui Zhang
    No:   500736315
    command run_tests to run all unit test cases or run_tests(fibonacci_sum) to execute individual test cases
    */

/*P Q1: first_missing_positive(Items, Result) */
first_missing_positive(Items, Result) :-
    first_missing_positive(Items, 1, Result).
first_missing_positive(Items, Result, Result) :-
    \+ member(Result, Items).
first_missing_positive(Items, N, Result) :-
    N1 is N+1,
    first_missing_positive(Result, N1, Items).

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

/* Q2: riffle(Left, Right, Result, Mode)*/ 
riffle([], [], [], _).
riffle(_, _, Result, _) :-
    length(Result, L),
    0 =:= mod(L, 2),!.
riffle([Left|Lefts], [Right|Rights], Result, left) :-
    riffle(Lefts, Rights, Result1, left),
    append([Left,Right], Result1, Result).
riffle([Left|Lefts], [Right|Rights], Result, right) :-
    riffle(Lefts, Rights, Result1, right),
    append([Right,Left], Result1, Result).
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

/*P Q3: sz(N, SZ)*/
seven_zero_rec(0, 0, 0).
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
seven_zero(0, 0, _, 0).
seven_zero(N, 0, Z, Acc) :-
    Z > 0,
    Acc > 0,
    plus(Z, -1, Z1),
    Acc1 is Acc*10,
    seven_zero(N, 0, Z1, Acc1).

sz(N, SZ) :-
    sz(N, SZ, 1).
sz(N, SZ, T) :- 
    sz(N, SZ, 1, T),!.
sz(N, SZ, T) :- 
    plus(T, 1, T1),
    sz(N, SZ, T1).
sz(N, SZ, SN, T) :-
    plus(SN, ZN, T),
    seven_zero(SZ, SN, ZN),
    0 =:= mod(SZ, N),!.
sz(N, SZ, SN, T) :-
    SN<T,
    plus(SN, 1, SN1),
    sz(N, SZ, SN1, T).

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

/*P Q4: crag(A, B, C, Score)*/
dice(1).
dice(2).
dice(3).
dice(4).
dice(5).
dice(6).
dice(A,B,C) :-
    dice(A),
    dice(B),
    dice(C).
crag_double([A, B, C], L) :-
    A =:= L,
    B =:= L,
    C =\= L.
crag_double([A, B, C], L) :-
    A =:= L,
    B =\= L,
    C =:= L.
crag_double([A, B, C], L) :-
    A =\= L,
    B =:= L,
    C =:= L.
crag([A, B, C], 50) :-
    dice(A,B,C),
    A+B+C =:= 13,
    (A =:= B; B =:= C; C =:= A).
crag([A, B, C], 26) :-
    dice(A,B,C),
     A =\= B,
     B =\= C,
     C =\= A,
    A+B+C =:= 13.
crag([A, B, C], 25) :-
    dice(A,B,C),
    A =:= B,
    B =:= C.
crag([A, B, C], 20) :-
    dice(A,B,C),
    msort([A, B, C], [1,2,3]).
crag([A, B, C], 20) :-
    dice(A,B,C),
    msort([A, B, C], [4,5,6]).
crag([A, B, C], 20) :-
    dice(A,B,C),
    msort([A, B, C], [1,3,5]).
crag([A, B, C], 20) :-
    dice(A,B,C),
    msort([A, B, C], [2,4,6]).
crag([A, B, C], 5) :-
    dice(A,B,C),
    msort([A, B, C], [3,4,5]).
crag([A, B, C], 4) :-
    dice(A,B,C),
    msort([A, B, C], [2,3,4]).
crag([A, B, C], 1) :-
    dice(A,B,C),
    msort([A, B, C], [1,3,4]).
crag([A, B, C], 12) :-
    dice(A,B,C),
    crag_double([A, B, C], 6).
crag([A, B, C], 10) :-
    dice(A,B,C),
    crag_double([A, B, C], 5).
crag([A, B, C], 8) :-
    dice(A,B,C),
    crag_double([A, B, C], 4).
crag([A, B, C], 6) :-
    dice(A,B,C),
    crag_double([A, B, C], 3).
crag([A, B, C], 4) :-
    dice(A,B,C),
    crag_double([A, B, C], 2).
crag([A, B, C], 3) :-
    dice(A,B,C),
    crag_double([A, B, C], 1).
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

/*P Q5: count_dominators(Items, Result)*/
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

/*P Q6: running_median(Items, Medians)*/
median(L, X) :-
    msort(L, SL),
    length(L, Len),
    Len2 is floor(Len/2),
    nth0(Len2, SL, X).

running_median([], []).
running_median(Items, Medians) :-
    append(H,[T], Items),
    running_median(H, Medians1),
    (length(Medians1,Len), Len<2 -> append(H, [T], Medians);
    append(_,[L1,L2], H),
    median([L1,L2,T], X),
    append(Medians1, [X], Medians)).

:- begin_tests(running_median).
    test(running_median5, [true(M == [1, 2, 2, 3, 4])]) :-
        running_median([1, 2, 3, 4, 5], M).
    test(running_median7, [true(M == [99, 42, 42, 42, 17, 18, 18])]) :-
        running_median([99, 42, 17, 55, -4, 18, 77], M).
    test(running_median42, [true(M == [42, 42, 42, 42, 42])]) :-
        running_median([42, 42, 99, 42, 42], M).
:- end_tests(running_median).

/*P Q7. safe_squares_rooks(Rooks, N, S)*/
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
    test(safe_squares_rooks4, [true(S == 4)]) :-
        safe_squares_rooks([(2, 2), (3, 1), (5, 5), (2, 5)], 5, S).
    test(safe_squares_rooks0, [true(S == 0)]) :-
        safe_squares_rooks([(1, 1), (2, 2), (3, 3), (4, 4), (5, 5)], 5, S). 
    test(safe_squares_rooks10000, [true(S == 10000)]) :-
        safe_squares_rooks([], 100, S).
:- end_tests(safe_squares_rooks).

/*P Q8. trick_winner(Cards, Winner)*/
suit(clubs).
suit(diamonds).
suit(hearts).
suit(spades).
rank(ace).
rank(king).
rank(queen).
rank(jack).
rank(ten).
rank(nine).
rank(eight).
rank(seven).
rank(six).
rank(five).
rank(four).
rank(trey).
rank(deuce).
resucc(ace,king).
resucc(king,queen).
resucc(queen,jack).
resucc(jack,ten).
resucc(ten,nine).
resucc(nine,eight).
resucc(eight,seven).
resucc(seven,six).
resucc(six,five).
resucc(five,four).
resucc(four,trey).
resucc(trey,deuce).
rank(X,Y) :-
    resucc(X,Y).
rank(X,Y) :-
    resucc(X, Z),
    rank(Z, Y).
higher_rank(R1, R2) :-
    rank(R1,R2).
higher_suit((_,S1),(_,S2)) :-
    S1 \== S2. 
higher_suit((C1,S1),(C2,S2)) :-
    S1 == S2,
    higher_rank(C1, C2).
valid_card((_,S1),(_,S2)) :-
    S1\==S2.
valid_card((C1,S1),(C2,S2)) :-
    S1==S2,
    C1\==C2.
valid_cards((C1,S1),(C2,S2),(C3,S3),(C4,S4)) :-
    valid_card((C1,S1),(C2,S2)),
    valid_card((C1,S1),(C3,S3)),
    valid_card((C1,S1),(C4,S4)),
    valid_card((C2,S2),(C3,S3)),
    valid_card((C2,S2),(C4,S4)),
    valid_card((C3,S3),(C4,S4)).
trick_winner(Cards, Winner) :-
    [(C1,S1),(C2,S2),(C3,S3),(C4,S4)]=Cards,
    (C,S)=Winner,
    suit(S1),
    suit(S2),
    suit(S3),
    suit(S4),
    suit(S),
    higher_suit((C1,S1),(C2,S2)),
    higher_suit((C1,S1),(C3,S3)),
    higher_suit((C1,S1),(C4,S4)),
    valid_cards((C1,S1),(C2,S2),(C3,S3),(C4,S4)),
    C=C1,
    S=S1.
trick_winner(Cards, Winner) :-
    [(C1,S1),(C2,S2),(C3,S3),(C4,S4)]=Cards,
    (C,S)=Winner,
    suit(S1),
    suit(S2),
    suit(S3),
    suit(S4),
    suit(S),
    S1 == S2,
    higher_rank(C2, C1),
    higher_suit((C2,S2),(C3,S3)),
    higher_suit((C2,S2),(C4,S4)),
    valid_cards((C1,S1),(C2,S2),(C3,S3),(C4,S4)),
    C=C2,
    S=S2.
trick_winner(Cards, Winner) :-
    [(C1,S1),(C2,S2),(C3,S3),(C4,S4)]=Cards,
    (C,S)=Winner,
    suit(S1),
    suit(S2),
    suit(S3),
    suit(S4),
    suit(S),
    S1 == S3,
    higher_rank(C3, C1),
    higher_suit((C3,S3),(C2,S2)),
    higher_suit((C3,S3),(C4,S4)),
    valid_cards((C1,S1),(C2,S2),(C3,S3),(C4,S4)),
    C=C3,
    S=S3.
trick_winner(Cards, Winner) :-
    [(C1,S1),(C2,S2),(C3,S3),(C4,S4)]=Cards,
    (C,S)=Winner,
    suit(S1),
    suit(S2),
    suit(S3),
    suit(S4),
    suit(S),
    S1 == S4,
    higher_rank(C4, C1),
    higher_suit((C4,S4),(C2,S2)),
    higher_suit((C4,S4),(C3,S3)),
    valid_cards((C1,S1),(C2,S2),(C3,S3),(C4,S4)),
    C=C4,
    S=S4.
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

/*P Q9. sum_of_distinct_cubes(N, L)*/
sum_of_distinct_cubes(N, R, [R]) :-
    N>0,
    N =:= R^3.
sum_of_distinct_cubes(N, R, L) :-
    N1 is N-R^3,
    N1>0,
    R1 is R-1,
    R1>0,
    sum_of_distinct_cubes(N1, R1, L1),
    append([R], L1, L).
sum_of_distinct_cubes(N, R, L) :-
    plus(R1, 1, R),
    R1>0,
    sum_of_distinct_cubes(N, R1, L),!.
sum_of_distinct_cubes(N, L) :-
    N>0,
    R is round(N^(1/3)),
    sum_of_distinct_cubes(N, R, L).

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

/*P Q10. fibonacci_sum(N, L)*/
fibs_upto(N, L, L) :-
    [Lf,Ls|_]=L,
    F is Lf+Ls,
    N<F.
fibs_upto(N, L1, L) :-
    [Lf,Ls|_]=L1,
    F is Lf+Ls,
    N>=F,
    append([F], L1, L2),
    fibs_upto(N, L2, L).
fibs_upto(N, L) :-
    fibs_upto(N, [1,1], L).

fibonacci_sum(N, _, L) :-
    N =:= 0,
    L=[].
fibonacci_sum(N, F, L) :-
    [H|T] = F,
    (H=<N ->
    N1 is N-H,
    fibonacci_sum(N1, T, L1),
    append([H], L1, L);
    fibonacci_sum(N, T, L)).
fibonacci_sum(N, L) :- 
   fibs_upto(N, F),
   fibonacci_sum(N, F, L).
    
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
