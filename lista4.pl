% zadanie 1
    
len1([], 0). 
len1([_|T], N) :- 
    integer(N), 
    N1 is N - 1, 
    len1(T, N1), !. 
len1([_|T], N1) :- 
    len1(T, N), 
    N1 is N + 1. 

len3([], 0). 
len3([_|T], N1) :- 
    len3(T, N), 
    N1 is N + 1. 

len4([], 0). 
len4([_|T], N) :- 
    N > 0, 
    N1 is N - 1, 
    len4(T, N1).

len2(L, N) :- 
    var(N),
    len3(L, N). 
len2(L, N) :- 
    nonvar(N),
    len4(L, N). 

length2([], A, A). 
length2([_|T], A, N):- 
    \+ A == N, 
    A1 is A + 1, 
    length2(T, A1, N). 
length2(L, N):- 
    length2(L, 0, N). 
    
length3([], A, A). 
length3([_|T], A, N):- 
    (var(N) -> 
        true;
        A < N),
    A1 is A + 1, 
    length3(T, A1, N). 
length3(L, N):- 
    length3(L, 0, N).


% zadanie 2

connection(wroclaw, warszawa).
connection(wroclaw, krakow).
connection(wroclaw, szczecin).
connection(szczecin, lublin).
connection(szczecin, gniezno).
connection(warszawa, katowice).
connection(gniezno, gliwice).
connection(lublin, gliwice).
connection(gliwice, wroclaw). 

% connection(a,b). 
% connection(b,c). 
% connection(c,d). 
% connection(d,c).

trip(X, Y, [X, Y], _) :- 
    connection(X, Y). 
trip(X, Y, [X|R], L) :- 
    \+ X == Y,
    connection(X, Z),
    \+ member(Z, L), 
    trip(Z, Y, R, [Z|L]).
trip(X, Y, R) :- 
    trip(X, Y, R, [X]). 
    
trip2(P, T, T) :-
    T = [P|_].
trip2(P, T, [H|A]) :-
    connection(X, H),
    \+ member(X, A),
    trip2(P, T, [X,H|A]).
trip2(P, K, T) :-
    trip2(P, T, [K]).
    
    
% zadanie 3

comb1([0], 1).
comb1([1], 1).
comb1([0|T], N) :-
    N > 1,
    N1 is N - 1,
    comb1(T, N1).
comb1([1|T], N) :-
    N > 1,
    N1 is N - 1,
    comb1(T, N1).
    
bin1([1|T], N) :-
    N1 is N - 1,
    comb1(T, N1).
bin1(T, N) :-
    N2 is N + 1,
    bin1(T, N2).
    
bin1([0]).
bin1([1]).
bin1(X) :-
    bin1(X, 2).
    
comb2([0], []). 
comb2([1], []). 
comb2([0|R], [_|T]) :-
    comb2(R, T). 
comb2([1|R], [_|T]) :-
    comb2(R, T). 
    
bin2([1|T], [_|S]) :-
    comb2(T, S).
bin2(T, S) :-
    bin2(T, [_|S]).
    
bin2([0]).
bin2([1]).
bin2(X) :-
    bin2(X, [_]).
    
single_bin(0). 
single_bin(1). 

bin_with_zero(A, A). 
bin_with_zero(R, A) :- 
    bin_with_zero(R, [B|A]), 
    single_bin(B). 
    
bin3([0]). 
bin3([1|T]) :- 
    bin_with_zero(T, []). 
    
rinc([], [1]). 
rinc([0|T], [1|T]). 
rinc([1|T], [0|T1]) :- 
    rinc(T, T1). 

rbin1([0]). 
rbin1(X) :- 
    rbin1(Z), 
    rinc(Z, X). 
    
list([_]). 
list([_|Y]) :- 
    list(Y). 
    
fill([]). 
fill([X|Xs]) :- 
    (X = 0; 
    X = 1), 
    fill(Xs).

bin([0]). 
bin([1]). 
bin([1|X]) :- 
    list(X),
    fill(X). 

rfill([1]). 
rfill([X|Xs]) :- 
    rfill(Xs), 
    (X = 0; 
    X = 1). 

rbin([0]).
rbin(X) :- 
    list(X),
    rfill(X).
    
digit(0).
digit(1).

xrbin1([1]).
xrbin1([D|X]) :-
    xrbin1(X),
    digit(D).
    
xrbin([0]).
xrbin(X) :-
    xrbin1(X).
    
xbin(A, A).
xbin(X, A) :-
    xbin(X, [D|A]),
    digit(D).
    
xbin([0]).
xbin([1|X]) :-
    xbin(X, []).
    
    
% zadanie 4

mirror(leaf, leaf). 
mirror(node(L, N, R), node(L1, N, R1)) :- 
    mirror(L, L1), 
    mirror(R, R1). 

flatten1(leaf, []). 
flatten1(node(L, N, R), A) :- 
    flatten1(L, LN), 
    flatten1(R, RN), 
    append(LN, [N|RN], A).
    
flatten2(leaf, A, A). 
flatten2(node(LT, N, RT), L1, A) :- 
    flatten2(RT, L, A),
    flatten2(LT, L1, [N|L]). 
flatten2(T, L) :- 
    flatten2(T, L, []). 
    
flatten3(leaf, L, L). 
flatten3(node(L, N, R), H, T):- 
    flatten3(L, H, [N|M]), 
    flatten3(R, M, T). 
flatten3(T, L) :- 
    flatten3(T, L, []). 
 
% mirror(node(leaf, 3, node(leaf, 5, leaf)), X).
% flatten(node(leaf, 3, node(leaf, 5, leaf)), X).


% zadanie 5

insert(D, leaf, node(leaf, D, leaf)). 
insert(D, node(LT, N, RT), node(LT1, N, RT)) :- 
    D =< N, 
    insert(D, LT, LT1). 
insert(D, node(LT, N, RT), node(LT, N, RT1)) :- 
    D > N, 
    insert(D, RT, RT1). 
    
mktree([],leaf). 
mktree([H|T], Tree) :- 
    mktree(T, TT), 
    insert(H, TT, Tree).

treesort1(L,S) :- 
    mktree(L, T), 
    flatten(T, S). 
    
treesort2([], D, D). 
treesort2([H|T], D, R) :- 
    insert(H, D, D1), 
    treesort2(T, D1, R). 
treesort2(L, S) :- 
    treesort2(L, leaf, R), 
    flatten(R, S). 


% zadanie 6

sublist([], []). 
sublist([H|T], [H|S]) :- 
    sublist(T, S). 
sublist([_|T], S) :- 
    sublist(T, S). 

concat_number([], A, A).
concat_number([H|T], A, R) :-
    A2 is 10 * A + H,
    concat_number(T, A2, R).
concat_number(D, N) :-
    concat_number(D, 0, N).
    
solve(A, C, E, P, R, S, U) :-
    length(Nums, 7),
    sublist([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], Nums), 
    permutation(Nums, [A, C, E, P, R, S, U]), 
    U \= 0, P \= 0, 
    concat_number([U, S, A], USA), 
    concat_number([U, S, S, R], USSR), 
    concat_number([P, E, A, C, E], PEACE), 
    PEACE is USA + USSR.

% solve(A, C, E, P, R, S, U).
