% zadanie 1

revall(X, X) :- 
    atomic(X), !. 
revall(X, Y) :- 
    revall(X, [], Y). 
    
revall([], A, A). 
revall([H|T], A, Y) :- 
    revall(H, H2),
    revall(T, [H2|A], Y).
    
revall2(Xs, Ys):-
    revall2(Xs, [], Ys). 

revall2([], X, X). 
revall2([H|T], X, W):- 
    atomic(H), !, 
    revall2(T, [H|X], W). 
revall2([H|T], X, W):- 
    revall2(H, Y), 
    revall2(T, [Y|X], W). 
    
revall3(X, Y) :-
    revall3(X, [], Y).
    
revall3([], A, A).
revall3([H|T], A, Y) :-
    (H = [_|_] ->
        revall3(H, H2);
        H2 = H),
    revall3(T, [H2|A], Y).
        
        
% zadanie 2

flatten(X, Y) :- 
    flatten(X, [], Y). 
    
flatten([], A, A). 
flatten([H|T], A, [H|R]) :- 
    atomic(H), !,
    flatten(T, A, R). 
flatten([H|T], A, R) :- 
    flatten(T, A, A2), 
    flatten(H, A2, R).
    
flatten2(X, Y) :- 
    flatten2(X, [], Y). 
    
flatten2([], A, A).
flatten2([H|T], A, R) :-
    flatten2(T, A, A2),
    flatten2(H, A2, R).
flatten2(N, [N|T], T) :-
    N \= [].
    
    
% zadanie 3

halve(X, L, R) :- 
    halve(X, X, L, R). 

halve(R, [], [], R). 
halve(R, [_], [], R). 
halve([X|XS], [_,_|YS], [X|L], R) :- 
    halve(XS, YS, L, R). 
    
half(X, L, R) :-
    half(X, L, [], R, []).
half([], A1, A1, A2, A2).
half([M], L, A1, R, A2) :-
    half([], L, A1, R, [M|A2]).
half([N,M|T], L, A1, R, A2) :-
    half(T, L, [N|A1], R, [M|A2]).


% zadanie 4

merge([], R, R). 
merge(L, [], L). 
merge([X|XS], [Y|YS], [X|R]) :- 
    X < Y,
    merge(XS, [Y|YS], R). 
merge([X|XS], [Y|YS], [Y|R]) :- 
    X >= Y,
    merge([X|XS], YS, R). 

mergesort([], []) :- !. 
mergesort([N], [N]) :- !. 
mergesort(X, Y) :- 
    halve(X, L, R), 
    mergesort(L, A), 
    mergesort(R, B), 
    merge(A, B, Y).
    
    
% zadanie 5

app(H1-H2, H2-T2, H1-T2).
rev([], DL-DL).
rev([H|T], H2-T2) :-
    rev(T, H2-[H|T2]).
    
put(N, H-[N|T], H-T).
get([N|H]-T, N, H-T).
empty(L-L).
    
empty2(X-Y) :-
    X == Y.
    
halve2(X-Y, X-Z, Z-Y) :-
    halve2(X-Y, X-Y, X-Z, Z-Y).
    
halve2(Z-Y, X-Y, _-Z, Z-Y) :-
    empty2(X-Y), !.
halve2(Z-Y, [_|X]-Y, _-Z, Z-Y) :-
    empty2(X-Y), !.
halve2([_|T]-Y, [_,_|T1]-Y, X-Z, Z-Y) :-
    halve2(T-Y, T1-Y, X-Z, Z-Y).
    
mergesort2(X-X, []) :- !.
mergesort2([H|X]-X, [H]) :- !.
mergesort2(X-Y, R) :-
    halve2(X-Y, A-B, C-D),
    mergesort2(A-B, P),
    mergesort2(C-D, Q),
    merge(P, Q, R).
    
msort2(X, Y) :-
    mergesort2(X-[], Y).
    
halve3(X-Y, X-Z, Z-Y) :-
    halve3(X-Y, X-Y, Z-Y).
    
halve3(X-X, L, L). % halve3(X-Y, L, L) :- X == Y, !.
halve3([_|X]-X, L, L). % halve3([_|X]-Y, L, L) :- X == Y, !.
halve3([_,_|X]-Xs, [_|Y]-Ys, L) :-
    halve3(X-Xs, Y-Ys, L).


% zadanie 6

split([], _, [], []). 
split([H|T], M, [H|S], B) :- 
    H < M, 
    split(T, M, S, B). 
split([H|T], M, S, [H|B]) :- 
    H >= M,
    split(T, M, S, B). 

qsort(X,Y) :- 
    qsort(X, [], Y). 
 
qsort([], A, A) :- !. 
qsort([N], A, [N|A]) :- !. 
qsort([H|T], A, R) :- 
    split(T, H, S, B), 
    qsort(B, A, A2), 
    qsort(S, [H|A2], R).
    