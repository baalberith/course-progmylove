% zadanie 1
    
len1([], 0). 
len1([_|A], X) :- 
    integer(X), 
    Y is X - 1, 
    len1(A, Y), !. 
len1([_|A], X) :- 
    len1(A, Y), 
    X is Y + 1. 

len3([],0). 
len3([_|T],N) :- 
    len3(T,N1), 
    N is N1 + 1. 

len4([],0). 
len4([_|T],N) :- 
    N > 0, 
    N1 is N - 1, 
    len4(T,N1).

len2(List,N) :- 
    var(N),
    len3(List,N). 
len2(List,N) :- 
    nonvar(N),
    len4(List,N). 


% zadanie 2

% connection(wroclaw, warszawa).
% connection(wroclaw, krakow).
% connection(wroclaw, szczecin).
% connection(szczecin, lublin).
% connection(szczecin, gniezno).
% connection(warszawa, katowice).
% connection(gniezno, gliwice).
% connection(lublin, gliwice).
% connection(gliwice, wroclaw). 

connection(a,b). 
connection(b,c). 
connection(c,d). 
connection(d,c).

trip(X, Y, [X, Y], _) :- 
    connection(X, Y). 
trip(X, Y, [X|R], L) :- 
    connection(X, Z),
    \+ X == Y,
    \+ member(Z, L), 
    trip(Z, Y, R, [Z|L]).
trip(X, Y, R) :- 
    trip(X, Y, R, [X]). 
    
    
% zadanie 3

comb([0], 1).
comb([1], 1).
comb([0|T], N) :-
    N > 1,
    N1 is N - 1,
    comb(T, N1).
comb([1|T], N) :-
    N > 1,
    N1 is N - 1,
    comb(T, N1).
    
bin([1|T], N) :-
    N1 is N - 1,
    comb(T, N1).
bin(T, N) :-
    N2 is N + 1,
    bin(T, N2).
    
bin([0]).
bin([1]).
bin(X) :-
    bin(X, 2).
    
    
% zadanie 4

mirror(leaf, leaf). 
mirror(node(A,D,B), node(B2,D,A2)) :- 
    mirror(A,A2), 
    mirror(B,B2). 

flatten(leaf, []). 
flatten(node(A,D,B), ACC) :- 
    flatten(A,AD), 
    flatten(B,BD), 
    append(AD,[D|BD],ACC).
    
% mirror(node(leaf, 3, node(leaf, 5, leaf)), X).
% flatten(node(leaf, 3, node(leaf, 5, leaf)), X).


% zadanie 5

insert(X, leaf, node(leaf,X,leaf)). 
insert(X, node(A,D,B),node(AP,D,B)) :- 
    X =< D, 
    insert(X,A,AP). 
insert(X, node(A,D,B),node(A,D,BP)) :- 
    X > D, 
    insert(X,B,BP). 
    
mktree([],leaf). 
mktree([H|T],Tree) :- 
    mktree(T,Bef), 
    insert(H,Bef,Tree).

treesort1(L,S) :- 
    mktree(L,T), 
    flatten(T,S). 
    
treesort2([], D, D). 
treesort2([H|T], D, W) :- 
    insert(H, D, D1), 
    treesort2(T, D1, W). 
treesort2(L, S) :- 
    treesort2(L, leaf, W), 
    flatten(W, S). 


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

% solve2(A, C, E, P, R, S, U).
