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

member(H, [H|_]) :- !.
member(N, [_|T]) :- 
    member(N, T).

trip(X, Y, [X, Y], _) :- 
    connection(X, Y). 
trip(X, Y, [X|R], L) :- 
    connection(X, Z),
    \+ X == Y,
    \+ member(Z, L), 
    trip(Z, Y, R, [Z|L]).
trip(X, Y, R) :- 
    trip(X, Y, R, [X]). 
    
    
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

treesort(L,S) :- 
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

sublist([],[]). 
sublist([H|S],[H|T]) :- 
    sublist(S,T). 
sublist([_|S],T) :- 
    sublist(S,T). 

digit(A) :- 
    member(A,[0,1,2,3,4,5,6,7,8,9]). 
positive(A) :- 
    member(A,[1,2,3,4,5,6,7,8,9]). 

concat_number([], A, A).
concat_number([H|T], A, R) :-
    A2 is 10 * A + H,
    concat_number(T, A2, R).
concat_number(D, N) :-
    concat_number(D, 0, N).
    
select(H, [H|T], T).
select(X, [H|T], [H|S]) :-
   select(X, T, S).
   
permutation1([], [], []).
permutation1([H|T], L, [_|Ls]) :-
    permutation1(T, PT, Ls),
    select(H, L, PT).
permutation1(X, Y) :-
    permutation1(X, Y, Y).

link(Vars,P) :- 
    sublist([0,1,2,3,4,5,6,7,8,9],X), 
    length(Vars,N), 
    length(X,N), 
    permutation(P,X). 

solve(Vars, P1, P2, R, S1, S2) :- 
    link(Vars,Perm), 
    Vars = Perm, 
    not(S1 = 0), 
    not(S2 = 0), 
    concat_number(P1,X), 
    concat_number(P2,Y), 
    Z is X + Y, 
    concat_number(R,Z). 
    
solve2(A,C,E,P,R,S,U) :- 
    L = [0,1,2,3,4,5,6,7,8,9], 
    permutation(L,[U,S,A,R,P,E,C,_,_,_]), 
    U \= 0, 
    P \= 0, 
    concat_number([U,S,A],USA), 
    concat_number([U,S,S,R],USSR), 
    concat_number([P,E,A,C,E],PEACE), 
    PEACE is USA + USSR,
    !.
    
solve3(A,C,E,P,R,S,U) :- 
    CYFRY = [0,1,2,3,4,5,6,7,8,9], 
    sublist(CYFRY,C7), 
    length(C7,7), 
    permutation(C7,[A,C,E,P,R,S,U]), 
    U \= 0, 
    P \= 0, 
    concat_number([U,S,A],USA), 
    concat_number([U,S,S,R],USSR), 
    concat_number([P,E,A,C,E],PEACE), 
    PEACE is USA + USSR, 
    !.
    
solve4(A,C,E,P,R,S,U) :- 
    CYFRY = [0,1,2,3,4,5,6,7,8,9], 
    sublist(CYFRY, [A1,A2,A3,A4,A5,A6,A7]), 
    permutation([A1,A2,A3,A4,A5,A6,A7], [A,C,E,P,R,S,U]), 
    U \= 0, 
    P \= 0, 
    concat_number([U,S,A],USA), 
    concat_number([U,S,S,R],USSR), 
    concat_number([P,E,A,C,E],PEACE), 
    PEACE is USA + USSR, 
    !.

% solve([A,C,E,P,R,S,U],[U,S,A],[U,S,S,R],[P,E,A,C,E],U,P).
% solve(A,C,E,P,R,S,U).
