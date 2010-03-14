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

count(_, [], 0).
count(H, [H|T], N2) :-
    count(H, T, N),
    N2 is N + 1.
count(E, [H|T], N) :-
    E \= H,
    count(E, T, N).
    
count2(E, L, Count):- 
    count2(E, L, 0, Count). 
count2(_, [], Curr, Curr). 
count(E, [E|T], Curr, Count) :- 
    Curr1 is Curr + 1, 
    count2(E, T, Curr1, Count). 
count(E, [H|T], Curr, Count):- 
    E \= H, 
    count2(E, T, Curr, Count).
    
% exp(+Base, +Exp, ?Res)

exp(_, 0, A, A).
exp(B, N, A, R) :-
    N > 0,
    N2 is N - 1,
    A2 is A * B,
    exp(B, N2, A2, R).
exp(B, E, R) :-
    exp(B, E, 1, R).
    
    
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

concat_number([N], A, R) :- 
    R is 10 * A + N, !.
concat_number([H|T], A, R) :-
    A2 is 10 * A + H,
    concat_number(T, A2, R).
concat_number(D, N) :-
    concat_number(D, 0, N).

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

reverse(X, Y) :- reverse(X, [], Y, Y). 
reverse([], A, A, []). 
reverse([X|Xs], A, Y, [_|Ls]) :- 
    reverse(Xs, [X|A], Y, Ls).
    
    
% zadanie 6
    
same_length([], []). 
same_length([_|T1], [_|T2]) :- same_length(T1, T2). 

perms([], []). 
perms(L, [E|PR]) :- 
    same_length(L, [E|PR]), 
    select(L, E, R), 
    perms(R, PR). 

permi([], []). 
permi([H|T], P) :- 
    same_length([H|T], P), 
    permi(T, PT), 
    select(P, H, PT).
    
perm_sel(_, _, [], []). 
perm_sel([X|Xs], Y, [_|L], [_|R]) :- 
    perm_sel([X|Xs], Y, L, R), 
    select(X, Y, Z), 
    perm_sel(Xs, Z, L, R). 
perm_sel(X, Y) :- 
    perm_sel(X, Y, X, Y). 

perm_ins(_, _, [], []). 
perm_ins([X|Xs], Y, [_|L], [_|R]) :- 
    perm_ins([X|Xs], Y, L, R), 
    perm_ins(Xs, Z), 
    select(X, Y, Z). 
perm_ins(X, Y) :- 
    perm_ins(X, Y, X, Y). 
    