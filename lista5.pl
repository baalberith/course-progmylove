% zadanie 1

revall(X, X) :- 
    atomic(X), !. 
revall(X, Y) :- 
    revall(X, [], Y). 
    
revall([], A, A). 
revall([H|T], A, Y) :- 
    revall(H, H2),
    revall(T, [H2|A], Y).
        
        
% zadanie 2

flatten(X, Y) :- 
    flatten(X, [], Y). 
    
flatten([], A, A). 
flatten([H|T], A, [H|R]) :- 
    atomic(H), 
    flatten(T, A, R). 
flatten([H|T], A, R) :- 
    flatten(T, A, A2), 
    flatten(H, A2, R).
    
    
% zadanie 3

halve(X, L, R) :- 
    halve(X, X, L, R). 

halve(R, [], [], R). 
halve(R, [_], [], R). 
halve([X|XS], [_,_|YS], [X|L], R) :- 
    halve(XS, YS, L, R). 


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
    