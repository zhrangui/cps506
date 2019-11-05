/*  CCPS 721 Midterm
    Name: Rui Zhang
    No:   500736315
    command run_tests to run all unit test cases or run_tests(fibonacci_sum) to execute individual test cases
    */

/*P Q1: first_missing_positive(Items, Result) */

less_than_second(F, S) :-
    integer(F),
    integer(S),
    F >= 0,
    F+1 < S.
less_than_second(F, S) :-
    integer(F),
    \+ integer(S).
    
first_missing_positive([H|_], M, Result) :-
    less_than_second(M, H),
    Result is M+1.
first_missing_positive([], M, Result) :-
    less_than_second(M, []),
    Result is M+1.
first_missing_positive([H|S], _, Result) :-
    first_missing_positive(S, H, Result).
first_missing_positive(Items, Result) :-
    sort([0|Items], [H|S]),
    first_missing_positive(S, H, Result),!.

/* Q2: riffle(Left, Right, Result, Mode)*/ 
riffle([], [], [], _) :- !.
riffle([Left|Lefts], [Right|Rights], Result, left) :-
    riffle(Lefts, Rights, Result1, left),
    append([Left,Right], Result1, Result).
riffle([Left|Lefts], [Right|Rights], Result, right) :-
    riffle(Lefts, Rights, Result1, right),
    append([Right,Left], Result1, Result).

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

/*P Q5: count_dominators(Items, Result)*/
count_dominators([], 0).
count_dominators(Items, Result) :-
count_dominators(Items, Result, _).
count_dominators([Item], 1, Item).
count_dominators([Item|Items], Result, Item) :-
    count_dominators(Items, Result1, Max),
    Item > Max,
    plus(Result1, 1, Result),!.
count_dominators([_|Items], Result, Max) :-
    count_dominators(Items, Result, Max).

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
