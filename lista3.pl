% zadanie 1

% filter(+LNum, ?LPos)

filter([], []).
filter([H|T], [H|S]) :-
    H >= 0,
    filter(T, S).
filter([H|T], S) :-
    H < 0,
    filter(T, S).
    
% count(+Elem, +List, ?Count)

count1(_, [], 0).
count1(H, [H|T], N2) :-
    count1(H, T, N),
    N2 is N + 1.
count1(E, [H|T], N) :-
    E \= H,
    count1(E, T, N).
    
count2(_, [], Curr, Curr). 
count2(E, [E|T], Curr, Count) :- 
    Curr1 is Curr + 1, 
    count2(E, T, Curr1, Count). 
count2(E, [H|T], Curr, Count):- 
    E \= H, 
    count2(E, T, Curr, Count).
count2(E, L, Count):- 
    count2(E, L, 0, Count). 
    
% exp(+Base, +Exp, ?Res)

exp(_, 0, A, A).
exp(B, N, A, R) :-
    N > 0,
    N2 is N - 1,
    A2 is A * B,
    exp(B, N2, A2, R).
exp(B, E, R) :-
    exp(B, E, 1, R).
    
fexp(_, 0, 1).
fexp(B, E, R) :-
    E > 0,
    F is E //2,
    fexp(B, F, Q),
    (E mod 2 =:= 0 ->
        R is Q * Q;
        R is Q * Q * B).
    
    
% zadanie 2

% factorial(+N, ?M)

factorial(0, 1).
factorial(N, M) :-
    N > 0,
    N2 is N - 1,
    factorial(N2, M2),
    M is M2 * N.
    
factorial2(0, A, A).
factorial2(N, A, M) :-
    N > 0,
    A2 is A * N,
    N2 is N - 1,
    factorial2(N2, A2, M).
factorial2(N, M) :-
    factorial2(N, 1, M).

% concat_number(+Digits, ?Num)

concat_number([], A, A).
concat_number([H|T], A, R) :-
    A2 is 10 * A + H,
    concat_number(T, A2, R).
concat_number([H|T], N) :-
    concat_number(T, H, N).

% decimal(+Num, ?Digits) 

decimal(0, A, A).
decimal(N, A, L) :-
    N > 0,
    M is N // 10,
    R is N mod 10,
    decimal(M, [R|A], L).
decimal(0, [0]).
decimal(N, L) :-
    N > 0,
    decimal(N, [], L).
    
    
% zadanie 3

% select_min(+NumList, ?Min, ?Rest)

select_min([N], N, []).
select_min([H|T], H, [M|R]) :-
    select_min(T, M, R),
    M >= H.
select_min([H|T], M, [H|R]) :-
    select_min(T, M, R),
    H > M.
    
sel_min([], A1, A1, A2, A2).
sel_min([H|T], A1, M, A2, R) :-
    H >= A1,
    sel_min(T, A1, M, [H|A2], R).
sel_min([H|T], A1, M, A2, R) :-
    H < A1,
    sel_min(T, H, M, [A1|A2], R).
sel_min([H|T], M, R) :-
    sel_min(T, H, M, [], R).
    
sel_sort([], []). 
sel_sort(L, [H|T]) :- 
    select_min(L, H, R), 
    sel_sort(R, T).  
    
    
% zadanie 4

% insert(+NumList, +Elem, ?Res)

insert([], E, [E]).
insert([H|T], E, [E,H|T]) :-
    H >= E.
insert([H|T], E, [H|R]) :-
    H < E,
    insert(T, E, R).
    
ins_sort([], []).
ins_sort([H|T], R) :-
    ins_sort(T, L),
    insert(L, H, R).

    
% zadanie 5

% reverse(?List, ?Rev)

reverse1([], [], []).
reverse1([H|T], S, [_|Ss]) :-
    reverse1(T, R, Ss),
    append(R, [H], S).
reverse1(X, Y) :-
    reverse1(X, Y, Y).

reverse2([], A, A, []). 
reverse2([X|Xs], A, Y, [_|Ls]) :- 
    reverse2(Xs, [X|A], Y, Ls).
reverse2(X, Y) :- 
    reverse2(X, [], Y, Y).    
    
    
% zadanie 6

% permutation(?List, ?Perm)

permutation1([], [], []).
permutation1([H|T], L, [_|Ls]) :-
    permutation1(T, PT, Ls),
    select(H, L, PT).
permutation1(X, Y) :-
    permutation1(X, Y, Y).

same_length([], []). 
same_length([_|T1], [_|T2]) :- 
    same_length(T1,T2). 

permutation2([], A, A, []). 
permutation2(L, A, Y, [H|T]) :- 
    same_length(L, [H|T]), 
    select(X, L, L1), 
    permutation2(L1, [X|A], Y, T). 
permutation2(X, Y):- 
    permutation2(X, [], Y, Y).
    
sel(X, [X|Ys], Ys, _). 
sel(X, [Y|Rs], [Y|Ys], [_|Xs]):- 
  sel(X, Rs, Ys, Xs). 
  
perm1([], []).
perm1(L1, [X|L3]) :-
    sel(X, L1, L2, _),
    perm1(L2, L3).
    
perm2([], []).
perm2([H|T], L) :-
    perm2(T, PT),
    sel(H, L, PT, _).
    