% zadanie 2

even([]). 
even([_,_|T]) :- even(T).

reverse1([], []).
reverse1([H|T], S) :-
    reverse1(T, R),
    append(R, [H], S).
    
reverse2([], X, X).
reverse2([H|T], X, Y) :-
    reverse2(T, [H|X], Y).
reverse2(X, Y) :-
    reverse2(X, [], Y).
    
palindrom(X) :-
    reverse2(X, X).
    
singleton([_]).

% zadanie 3

head(H, [H|_]).

last([H], H).
last([_|T], H) :-
    last(T, H).
    
tail(T, [_|T]).

init([_], []).
init([H|T], [H|L]) :- 
    init(T, L).
    
prefix([], _).
prefix([H|T], [H|L]) :- 
    prefix(T, L).
    
suffix(L, L).
suffix([_|T], L) :- 
    suffix(T, L).
    
% zadanie 4
    
sublist([],[]). 
sublist([H|T1],[H|T2]):- 
    sublist(T1,T2). 
sublist([_|T1],T2):- 
    sublist(T1,T2). 
    
% zadanie 5

permutation1([], []).
permutation1(L1, [X|L3]) :-
    select(X, L1, L2),
    permutation1(L2, L3).
    
% zadanie 6

permutation2([], []).
permutation2([H|T], L) :-
    permutation(T, PT),
    select(H, L, PT).
    