CCPS 721 Example Prolog Programs


Rosetta Code (chosen by the instructor from Category:Prolog)


Fizzbuzz: https://rosettacode.org/wiki/FizzBuzz#Prolog
Guess the number: https://rosettacode.org/wiki/Guess_the_number#Prolog
Fibonacci sequence: https://rosettacode.org/wiki/Fibonacci_sequence#Prolog
Mandelbrot set: https://rosettacode.org/wiki/Mandelbrot_set#Prolog
Quicksort: https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Prolog


Prefix, suffix, sublist:


prefix(L1, L2) :- append(L1, _, L2).
suffix(L1, L2) :- append(_, L1, L2).
sublist([H|L1], L2) :-
    prefix([H|L1], L2).
sublist(L1, [_|L2]) :-
    sublist(L1, L2).


Check if list is strictly ascending:


is_ascending([]).
is_ascending([_]) :- !.
is_ascending([X, Y | T]) :-
    X < Y,
    is_ascending([Y|T]).


Maximum element of list:


/* recursive version */
maxlist_rec([X], X).
maxlist_rec([H|T], X) :-
    maxlist_rec(T, M),
    X is max(H, M).


/* tail-recursive accumulator version */
maxlist_acc([H|T], X) :- 
    maxlist_acc(T, H, X).
maxlist_acc([], X, X).
maxlist_acc([H|T], M, X) :-
    M2 is max(H, M),
    maxlist_acc(T, M2, X).


Reversing a list:


/* naive recursion */
my_rev([], []).
my_rev([H|T], R) :- my_rev(T, R2), append(R2, [H], R).


/* again, better with accumulator and tail recursion */
my_rev_acc(L, R) :- my_rev_acc(L, [], R).
my_rev_acc([], R, R).
my_rev_acc([H|T], Acc, R) :- my_rev_acc(T, [H|Acc], R).


/* Measure the logical inferences needed to reverse list each way. */
measure(N, I, J) :-
    findall(X, between(1, N, X), L),
    statistics(inferences, I1),
    my_rev(L, _), /* Shlemiel O(n^2) */
    statistics(inferences, I2),
    I is I2 - I1,
    statistics(inferences, I3),
    my_rev_acc(L, _), /* Linear O(n) */
    statistics(inferences, I4),
    J is I4 - I3.


Check whether two lists are cyclic shifts of each other:


cyclic_shift([], []).
cyclic_shift([H|L1], L2) :-
    append([H|Left], Right, [H|L1]),
    append(Right, [H|Left], L2). 


Forward differences of list:


forward_difference([], []) :- !.
forward_difference([_], []) :- !.
forward_difference([X, Y | T], [D | T2]) :-
    plus(X, D, Y),
    forward_difference([Y | T], T2).


Generate all subsequences of given list:


subsequence([], []).
subsequence([H|T], [H|T2]) :-
    subsequence(T, T2).
subsequence([_|T], T2) :-
    subsequence(T, T2).


Test whether given list has zigzag shape:


zigzag([]).
zigzag([_]).
zigzag([X, Y]) :- X \= Y, !.
zigzag([X, Y, Z | T]) :-
    X < Y, !, Y > Z, !, zigzag([Y, Z | T]).
zigzag([X, Y, Z | T]) :-
    X > Y, Y < Z, zigzag([Y, Z | T]).


Parity partition of list items:


pp([], [], []).
pp([H|T], [H|L], R) :-
    H mod 2 =:= 0,
    pp(T, L, R).
pp([H|T], L, [H|R]) :-
    H mod 2 =:= 1,
    pp(T, L, R).


Mergesort (with a whole bunch of green cuts):


split([], [], []) :- !.
split([X], [X], []) :- !.
split([X,Y|T], [X|T1], [Y|T2]) :-
    split(T, T1, T2).


merge([],[],[]) :- !.
merge([H|T],[],[H|T]) :- !.
merge([],[H|T],[H|T]) :- !.
merge([H1|T1],[H2|T2],[H1|T]) :-
    H1 < H2, !,
    merge(T1,[H2|T2],T).
merge([H1|T1],[H2|T2],[H2|T]) :-
    merge([H1|T1],T2,T).


mergesort([],[]) :- !.
mergesort([X],[X]) :- !.
mergesort([H1,H2|T], S) :-
    split([H1,H2|T], L1, L2),
    mergesort(L1, S1),
    mergesort(L2, S2),
    merge(S1, S2, S).


Falling power:


falling_power(_, 0, 1) :- !.
falling_power(X, N, P) :-
    N > 0,
    X2 is X - 1,
    N2 is N - 1,
    falling_power(X2, N2, P2),
    P is X * P2.


Falling power with accumulator tail recursion:


falling_power(X, N, P) :-
    falling_power_acc(X, N, 1, P).
falling_power_acc(_, 0, P, P) :- !.
falling_power_acc(X, N, Acc, P) :-
    N > 0,
    X2 is X - 1,
    N2 is N - 1,
    Acc2 is Acc * X,
    falling_power_acc(X2, N2, Acc2, P).




Primality testing:


is_prime(X) :- X < 2, !, fail.
is_prime(2) :- !.
is_prime(X) :- X mod 2 =:= 0, !, fail.
is_prime(X) :- is_prime(X, 3).
is_prime(X, D) :-
    D2 is D * D,
    D2 > X,
    !.
is_prime(X, D) :-
    X mod D > 0,
    D2 is D + 2,
    is_prime(X, D2).




Fibonacci numbers:


/* recursive version */
fib_rec(0, 1) :- !.
fib_rec(1, 1) :- !.
fib_rec(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_rec(N1, F1),
    fib_rec(N2, F2),
    F is F1 + F2.


/* accumulator version */
fib_acc(0, 1) :- !.
fib_acc(1, 1) :- !.
fib_acc(N, F) :- fib_acc(N, 1, 1, F).
fib_acc(1, _, F, F) :- !.
fib_acc(N, F2, F1, F) :-
    N > 0,
    FF is F2 + F1,
    N2 is N - 1,
    fib_acc(N2, F1, FF, F).


Collatz sequence:


collatz(1,[1]) :- !.
collatz(N, [N|L]) :-
    N > 1,
    1 is N mod 2, !,
    N2 is 3*N+1,
    collatz(N2, L).


collatz(N, [N|L]) :-
    N > 1,
    0 is N mod 2,
    N2 is N // 2,
    collatz(N2, L).


longest_collatz_rec(S, S, R) :-
    !,
    collatz(S, L),
    length(L, R).


longest_collatz_rec(S, E, R) :-
    S < E,
    S2 is S + 1,
    longest_collatz_rec(S2, E, R1),
    collatz(S, L),
    length(L, R2),
    R is max(R1, R2).


longest_collatz_acc(S, E, R) :-
    longest_collatz_acc(S, E, R, 0).


longest_collatz_acc(S, E, R, R) :-
    S > E,
    !.


longest_collatz_acc(S, E, R, BestSoFar) :-
    S =< E,
    collatz(S, L),
    length(L, Len),
    NewBest is max(Len, BestSoFar),
    S2 is S + 1,
    longest_collatz_acc(S2, E, R, NewBest).


Some useful functional programming operators implemented in Prolog


my_fold(X, _, [], X) :- !. /* sometimes also called "reduce" */
my_fold(X, P, [H|T], Y) :-
    call(P, X, H, Z),
    my_fold(Z, P, T, Y).


my_filter(_, [], []) :- !.
my_filter(P, [H|T], [H|T2]) :-
    call(P, H), !,
    my_filter(P, T, T2).
my_filter(P, [_|T], T2) :-
    my_filter(P, T, T2).


my_map(_, [], []) :- !.
my_map(P, [H|T], [H2|T2]) :-
    call(P, H, H2),
    my_map(P, T, T2).


my_take_while(_, [], []) :- !.
my_take_while(P, [H|T], [H|T2]) :-
    call(P, H), !,
    my_take_while(P, T, T2).
my_take_while(_, [_|_], []).




Automated testing and measuring of given query:


/*
Query        The query to test for correctness.
X        The expression to solve the query for.
Inf        The number of inferences needed to solve the query.
Res        List of all results found for the Query.
Test        An arbitrary query that has to be true for Result for test to pass.    
*/


test(Query, X, Inf, Res, Test) :-
    statistics(inferences, I1), /* SWI-Prolog built-in predicate */
    call(findall(X, Query, Res)),
    statistics(inferences, I2),
    Inf is I2 - I1,
    call(Test) -> /* Prolog shorthand for if-then-else */
            (write('success'), nl, !) ;
            (write('failure'), nl, fail).
test(_, _, 0, _, _).




Mathematica-style term rewriting:


rewrite(T, X, Y, Y) :- T == X, !.
rewrite(T, _, _, T) :- var(T), !.
rewrite(T, X, Y, R) :-
    T =.. [F|Args],
    rewrite_list(Args, X, Y, Ar),
    R =.. [F|Ar].


rewrite_list([],_,_,[]).
rewrite_list([H|T], X, Y, [H2|T2]) :-
    rewrite(H, X, Y, H2),
    rewrite_list(T, X, Y, T2).


Iterative deepening depth first search:


/* For demo purposes, Collatz state space as a graph. */


step(X, Y) :- X < 2**30, Y is 2*X.
step(X, Y) :- Y is (X-1) // 3, X =:= 3*Y+1, 1 =:= Y mod 2.


goal(30).


/* Depth-limited depth-first search. */
dldfs(X, L, D) :- dldfs(X, L, [], D).
dldfs(X, [X], _, _) :- goal(X), !.
dldfs(X, [X|L], V, D) :-
    D > 0,
    D2 is D - 1,
    step(X, Y),
    not(member(Y, [X|V])),
    dldfs(Y, L, [X|V], D2).


/* Iterative deepening DFS. */


idfs(X, L, M) :- idfs(X, L, 1, M).
idfs(X, L, D, _) :- dldfs(X, L, [], D), !.
idfs(X, L, D, M) :-
    D < M,
    D2 is D+1,
    write('Setting upper bound to '), write(D2), nl,
    idfs(X, L, D2, M).


Simplification of fractions, followed by building expression to produce given value:


sign(0, 0).
sign(X, -1) :- X < 0.
sign(X, 1) :- X > 0.


simp(X, X/1) :- integer(X).
simp(N1/D1, N/D) :-
    integer(N1),
    integer(D1), 
    !,
    sign(D1, S),
    G is gcd(N1, D1),
    N is S*N1/G,
    D is S*D1/G.


simp(T1 + T2, NR/DR) :- 
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1*D2 + N2*D1,
    D is D1 * D2,
    simp(N/D, NR/DR).


simp(T1 - T2, NR/DR) :- 
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1*D2 - N2*D1,
    D is D1 * D2,
    simp(N/D, NR/DR).
 
simp(T1 * T2, NR/DR) :-
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1 * N2,
    D is D1 * D2,
    simp(N/D, NR/DR).


simp(T1 / T2, NR/DR) :-
    simp(T1, N1/D1),
    simp(T2, N2/D2),
    N is N1 * D2,
    D is D1 * N2,
    simp(N/D, NR/DR).




binterm(E1, E2, (E1)+(E2)).
binterm(E1, E2, (E1)-(E2)).
binterm(E1, E2, (E1)*(E2)).
binterm(E1, E2, (E1)/(E2)) :-
    not(simp(E2, 0/_)).


make_expr([X], X) :- !.
make_expr(L, E) :-
    append([X|Left], [Y|Right], L),
    make_expr([X|Left], E1),
    make_expr([Y|Right], E2),
    binterm(E1, E2, E).


sublist([], _).
sublist([X|L], [X|L2]) :- sublist(L, L2).
sublist([X|L], [_|L2]) :- sublist([X|L], L2).


solve(L, E, Goal) :-
    simp(Goal, G),
    /* generate */
    sublist(Ls, L),
    permutation(Ls, L2),
    make_expr(L2, E),
    /* test */ 
    simp(E, G).


Minesweeper framework:


:- dynamic(ms_isopen/1).
height(15).
width(11).
ms_isopen((6,10)).


ms_ismine((1,2)).
ms_ismine((1,3)).
ms_ismine((1,9)).
ms_ismine((1,10)).
ms_ismine((1,11)).
ms_ismine((1,12)).


ms_ismine((2,1)).
ms_ismine((2,4)).
ms_ismine((2,7)).
ms_ismine((2,9)).
ms_ismine((2,10)).
ms_ismine((2,12)).
ms_ismine((2,15)).


ms_ismine((3,1)).
ms_ismine((3,7)).
ms_ismine((3,8)).
ms_ismine((3,9)).


ms_ismine((4,1)).
ms_ismine((4,7)).
ms_ismine((4,9)).
ms_ismine((4,13)).
ms_ismine((4,15)).


ms_ismine((5,1)).
ms_ismine((5,13)).


ms_ismine((6,5)).
ms_ismine((6,14)).
ms_ismine((6,15)).


ms_ismine((7,6)).


ms_ismine((8,7)).
ms_ismine((8,8)).
ms_ismine((8,9)).
ms_ismine((8,10)).
ms_ismine((8,13)).
ms_ismine((8,15)).


ms_ismine((9,1)).
ms_ismine((9,4)).
ms_ismine((9,15)).


ms_ismine((10,1)).
ms_ismine((10,15)).


ms_ismine((11,4)).
ms_ismine((11,6)).
ms_ismine((11,8)).
ms_ismine((11,11)).
ms_ismine((11,12)).
ms_ismine((11,15)).


ms_ismarked((-1, -1)).


inside((X,Y)) :- width(W), height(H), between(1, W, X), between(1, H, Y).


neighbour((X, Y), (Xn, Yn)) :- 
    inside((X, Y)), 
    member(D1, [-1,0,1]), 
    member(D2, [-1,0,1]), 
    (D1, D2) \= (0,0), 
    Xn is X + D1,
    Yn is Y + D2,
    inside((Xn, Yn)).


:- dynamic ms_ismarked/1.
:- dynamic ms_score/1.
:- dynamic ms_error.


ms_score(0).


isopen(C, N) :- 
    ms_isopen(C), 
    findall(Cn, (neighbour(C, Cn), ms_ismine(Cn)), L), 
    length(L, N).


ismarked(C) :- ms_ismarked(C).


opencell(C) :-
    ms_isopen(C), !.


opencell(C) :-
    ms_ismine(C), !,
    write('You hit a mine at '), write(C), nl,
    assertz(ms_error),
    fail.


opencell(C) :-
    not(inside(C)), !,
    write(C), write(' is outside the bounds'), nl,
    assertz(ms_error),
    fail.


opencell(C) :-
    assertz(ms_isopen(C)),
    write('Opening cell '), write(C), nl,
    ms_addscore.


ms_addscore :-
    ms_score(S),
    retract(ms_score(S)),
    S2 is S + 1,
    assertz(ms_score(S2)).


markcell(C) :-
    ms_ismarked(C), !.


markcell(C) :-
    not(ms_ismine(C)), !,
    write(C), write(' is not a mine'), nl,
    assertz(ms_error),
    fail.


markcell(C) :- 
    not(inside(C)), !,
    write(C), write(' is outside the bounds'), nl,
    assertz(ms_error),
    fail.


markcell(C) :-
    assertz(ms_ismarked(C)),
    write('Marking cell '), write(C), nl,
    ms_addscore.


drawcell(C) :-
    isopen(C,N), !,
    (N > 0 ->   write(N) ; write(' ')).


drawcell(C) :-
    ms_ismarked(C), !, write('M').


drawcell(_) :- write('*').


drawfield :-
    width(W),
    height(H),
    between(1, H, Y),
    between(1, W, X),
    drawcell((X,Y)),
    (   X =:= W ->   nl ; true),
    fail.


drawfield.


Some binary search tree operations.


bst_add(nil, X, tr(nil, X, nil)) :- !.
bst_add(tr(T1, X, T2), X, tr(T1, X, T2)).
bst_add(tr(T1, R, T2), X, tr(T3, R, T2)) :- X < R, bst_add(T1, X, T3).
bst_add(tr(T1, R, T2), X, tr(T1, R, T3)) :- X > R, bst_add(T2, X, T3).


bst_contains(tr(T1, R, _), X) :- 
    nonvar(R), nonvar(X), X < R, bst_contains(T1, X).
bst_contains(tr(T1, _, _), X) :- 
    var(X), bst_contains(T1, X).
bst_contains(tr(_, X, _), X).
bst_contains(tr(_, R, T2), X) :- 
    nonvar(R), nonvar(X), X > R, bst_contains(T2, X).
bst_contains(tr(_, _, T2), X) :- 
    var(X), bst_contains(T2, X).


bst_min(tr(nil, X, _), X) :- !.
bst_min(tr(T1, _, _), X) :- bst_min(T1, X).


/* Accumulators strike again with BST creation. */
bst_create(L, T) :- bst_create(L, nil, T).
bst_create([], T, T).
bst_create([H|T], CT, R) :- 
    bst_add(CT, H, CT2), bst_create(T, CT2, R).


/* Prolog does not have infinity for integers, so let us define one. */
lt(-inf, X) :- integer(X), !.
lt(X, inf) :- integer(X), !.
lt(X, Y) :- X < Y.


bst_verify(T) :- bst_verify(T, -inf, inf).
bst_verify(nil, _, _).
bst_verify(tr(T1, X, T2), Min, Max) :-
    lt(Min, X),
    lt(X, Max),
    bst_verify(T1, Min, X),
    bst_verify(T2, X, Max).


Delete element at every position given in position list:


delat(Pos, L, Res) :- delat(Pos, L, Res, 1).
delat(_,[],[],_).
delat([Ph|Pt],[_|T],T2,Ph) :- !, Pn is Ph + 1, delat(Pt, T, T2, Pn).
delat(Pos,[H|T],[H|T2],Off) :- Off2 is Off + 1, delat(Pos,T,T2,Off2).


General cryptarithmetic solver:


/* Checks if all elements of list are strictly different. */
strict_neq([]).
strict_neq([H|T]) :-
    strict_nmbr(H, T),
    strict_neq(T).


/* Checks that X is not identical to any value in the list. */
strict_nmbr(_, []).
strict_nmbr(X, [H|T]) :-
    X \== H,
    strict_nmbr(X, T).


/* Checks that X occurs in the list just once. */
just_once(_, []).
just_once(X, [H|T]) :-
    X == H, !,
    strict_nmbr(X, T).
just_once(X, [_|T]) :- just_once(X, T).


/* Collects to Vars all free variables in lists L1, L2 and L3. */
vars(L1, L2, L3, Vars) :-
    append(L1, L2, LL),
    append(LL, L3, L),
    setof(X, (member(X, L), var(X)), Vars).


/* Solve a cryptarithmetic problem. */
solve(First, Second, Sum) :-
    reverse(First, FirstR),
    reverse(Second, SecondR),
    reverse(Sum, SumR),
    vars(First, Second, Sum, Vars),
    solve(FirstR, SecondR, SumR, 0, Vars),
    strict_neq(Vars).


/* Base cases of the recursion. */
solve([], L, L, 0, _) :- !.
solve(L, [], L, 0, _) :- !.
solve([], [], [1], 1, _) :- !.
solve([], Second, Sum, 1, Vars) :- solve([1], Second, Sum, 0, Vars).
solve(First, [], Sum, 1, Vars) :- solve(First, [1], Sum, 0, Vars).


/* Solve with first digits H1, H2, H3 and carry C. */
solve([H1|T1], [H2|T2], [H3|T3], C, Vars) :-
    between(0, 9, H1),
    just_once(H1, Vars),
    between(0, 9, H2),
    just_once(H2, Vars),
    plus(H1, H2, S1),
    plus(S1, C, S),
    add(S, H3, T1, T2, T3, Vars).


/* Digits and carry add up to a single digit. */
add(H3, H3, T1, T2, T3, Vars) :-
    H3 < 10,
    just_once(H3, Vars),
    solve(T1, T2, T3, 0, Vars).


/* Digits and carry add up to two-digit number with carry. */ 
add(S, H3, T1, T2, T3, Vars) :-
    S > 9,
    plus(H3, 10, S),
    just_once(H3, Vars),
    solve(T1, T2, T3, 1, Vars).


Polynomial addition and multiplication:


/* Eliminate terms with zero coefficient. */
simp([], []).
simp([(0,_)|T], T2) :- !, simp(T, T2).
simp([H|T],[H|T2]) :- simp(T, T2).


/* Add two polynomials. */
polyadd([], L, L) :- !.
polyadd(L, [], L) :- !.
polyadd([(B1,E)|T1], [(B2,E)|T2], [(B,E)|T]) :- !,
    plus(B1, B2, B),
    polyadd(T1, T2, T).
polyadd([(B1,E1)|T1], [(B2,E2)|T2], [(B1,E1)|T]) :-
    E1 < E2,!,
    polyadd(T1, [(B2,E2)|T2], T).
polyadd([(B1,E1)|T1], [(B2,E2)|T2], [(B2,E2)|T]) :-
    E1 > E2,!,
    polyadd([(B1,E1)|T1], T2, T).
    
/* Multiply two polynomials. */
polymul([], _, []) :- !.
polymul([(B1,E1)|T1], [(B2,E2)], [(B,E)|T]) :-
    !,
    B is B1 * B2,
    plus(E1, E2, E),
    polymul(T1, [(B2, E2)], T).


polymul(P1, [(B2,E2)|T2], P) :-
    polymul(P1, [(B2, E2)], P2),
    polymul(P1, T2, P3),
    polyadd(P2, P3, P).


Bisection of a list of integers into two sublists of equal sums:


sum(L, A) :- sum(L, 0, A).
sum([], A, A).
sum([H|T], Acc, S) :- Acc2 is Acc + H, sum(T, Acc2, S).


bisect(L, L1, L2) :-
    sum(L, S),
    S2 is S // 2,
    bisect(L, L1, L2, S2, S2).


bisect([], [], [], 0, 0).
bisect([H|T], [H|L1], L2, S1, S2) :-
   plus(S11, H, S1),
   bisect(T, L1, L2, S11, S2).
bisect([H|T], L1, [H|L2], S1, S2) :-
   plus(S22, H, S2),
   bisect(T, L1, L2, S1, S22).


Single machine job shop scheduling problem:


jobshop(Jobs, Chosen, Profit) :- jobshop(Jobs, Chosen, 0, Profit).
jobshop([], [], _, 0).
jobshop([(S,E,P)|T], [(S,E,P)|With], Time, Profit) :-
    S >= Time,
    jobshop(T, With, E, P2),
    Profit is P2 + P,
    jobshop(T, _, Time, P3),
    Profit > P3,
    !.
jobshop([_|T], Chosen, Time, Profit) :-
    jobshop(T, Chosen, Time, Profit).


N queens solver (adapted from Rosetta Code):


nqueens(L, N) :- nqueens(L, N, N).


nqueens([], _, 0).
nqueens([X|L], N, R) :-
    R > 0,
    R2 is R - 1,
    nqueens(L, N, R2),
    between(1, N, X),
    noattack(X, L, 1).


noattack(_, [], _).
noattack(X, [Y|L], D) :-
    X =\= Y,
    X - Y =\= D,
    Y - X =\= D,
    D2 is D + 1,
    noattack(X, L, D2).


Constraint logic programming over integers


:- use_module(library(clpfd)).


/* Determine if (X, Y, Z) could be side lengths of a triangle. */
triangle(X, Y, Z) :-
    X #> 0,
    Y #> 0,
    Z #> 0,
    X + Y #> Z,
    X + Z #> Y,
    Y + Z #> X.


/* Determine whether a list is strictly ascending. */
is_ascending([]).
is_ascending([_]).
is_ascending([N1, N2 | T]) :-
    N1 #< N2,
    is_ascending([N2 | T]).


/* Factorials. */
n_factorial(0, 1).
n_factorial(N, F) :-
        N #> 0,
        N1 #= N - 1,
        F #= N * F1,
        n_factorial(N1, F1).


/* List of prime factors of the given integer N. */
factors(N, F) :-
    factors(N, F, 2).
factors(1, [], _).
factors(N, [N], First) :-
    N #> 1,
    First #> N.
factors(N, [A|F], First) :-
    N #> 1,
    two_factors(N, A, B, First),
    factors(B, F, A).


two_factors(N, A, B) :-
    two_factors(N, A, B, 1).


two_factors(N, A, B, First) :-    
    A #>= First,     
    B #>= A,
    A*A #=< N,
    B #< N,
    A * B #= N,
    indomain(A), 
    !. /* Eliminate the cut to produce all two-factor breakdowns. */


two_factors(N, N, 1, _).