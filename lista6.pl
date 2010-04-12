% zadanie 1
   
plus(X, Y, Z) :-
    Z is X + Y.
minus(X, Y, Z) :-
    Z is X - Y.
    
n(0).
n(X) :-
    n(X, 0).
n(X, N) :-
    N =< 0,
    X is (-1) * N + 1.
n(X, N) :-
    N =< 0,
    M is (-1) * N + 1,
    n(X, M).
n(X, N) :-
    N > 0,
    X is (-1) * N.
n(X, N) :-
    N > 0,
    M is (-1) * N,
    n(X, M).
    
sum(X, Y, Z) :-
    nonvar(X),
    nonvar(Y), !,
    plus(X, Y, Z).
sum(X, Y, Z) :-
    nonvar(X),
    nonvar(Z), !,
    minus(Z, X, Y).
sum(X, Y, Z) :-
    nonvar(Y),
    nonvar(Z), !,
    minus(Z, Y, X).
sum(X, Y, Z) :-
    nonvar(Z), !,
    n(X),
    minus(Z, X, Y).
sum(X, Y, Z) :-
    nonvar(X), !,
    n(Y),
    plus(X, Y, Z).
sum(X, Y, Z) :-
    nonvar(Y), !,
    n(X),
    plus(X, Y, Z).
sum(X, Y, Z) :-
    n(X), 
    n(Y),
    plus(X, Y, Z).
    
    
% zadanie 2

app(H1-H2, H2-T2, H1-T2).
    
find_next(P, N, N) :-
    is_prime(P, N), !.
find_next(P, N, M) :-
    N1 is N + 1,
    find_next(P, N1, M).
    
is_prime(X-Y, _) :- 
    X == Y, !.
is_prime([P|Ps]-Y, N) :-
    R is N mod P,
    R \= 0,
    is_prime(Ps-Y, N).

prime(2).
prime(N) :-
    prime([2|Y]-Y, 2, N).
prime(Primes, P, N) :-
    NextPrime is P + 1,
    find_next(Primes, NextPrime, N).
prime(Primes, P, M) :-
    NextPrime is P + 1,
    find_next(Primes, NextPrime, N),
    app(Primes, [N|Z]-Z, Ps),
    prime(Ps, N, M).
    

% zadanie 3

empty1([]).
put1(E, S, [E|S]).
get1([E|R], E, R).

addall1(E, G, _, _) :-
    call(G),
    assertz(solutions1(E)),
    fail.
addall1(_, _, S, R) :-
    collect1(S, R).
    
collect1(A, R) :-
    retract(solutions1(E)), !,
    collect1([E|A], R).
collect1(A, A).

empty2(X-X).
is_empty2(X-Y) :- X == Y.
put2(N, H-[N|T], H-T).
get2([N|H]-T, N, H-T).

addall2(E, G, _, _) :-
    call(G),
    assertz(solutions2(E)),
    fail.
addall2(_, _, S, R) :-
    collect2(S, R).

collect2(H-[E|T], R) :-
    retract(solutions2(E)), !,
    collect2(H-T, R).
collect2(A, A).


% zadanie 4

e(1, 2).
e(1, 3).
e(1, 5).
e(2, 4).
e(3, 5).
e(3, 6).
e(4, 5).
e(5, 6).
e(5, 7).

start(1).
goal(7).

dfs1(V, [V]) :-
    goal(V), !.
dfs1(V, [V|Rest]) :-
    e(V, V2),
    dfs1(V2, Rest).
    
dfs1(R) :-
    start(S),
    dfs1(S, R).
    
dfs2(S, Path, Path) :-
    goal(S).
dfs2(S, Visited, Path) :-
    e(S, S2),
    \+ member(S2, Visited),
    dfs2(S2, [S2|Visited], Path).
    
dfs2(P) :-
    start(S),
    dfs2(S, [S], Path),
    reverse(Path, P).
    
dfs(Path) :-
    start(V),
    empty1(S),
    put1(V, S, Stack),
    dfs(Stack, Path).
    
dfs(Stack, []) :-
    empty1(Stack), !.
dfs(Stack, [V|Path]) :-
    get1(Stack, V, St),
    addall1(V1, e(V, V1), St, S),
    dfs(S, Path).
    
bfs(Path) :-
    start(V),
    empty2(Q),
    put2(V, Q, Queue),
    bfs(Queue, Path).
    
bfs(Queue, []) :-
    is_empty2(Queue), !.
bfs(Queue, [V|Path]) :-
    get2(Queue, V, Qu),
    addall2(V1, e(V, V1), Qu, Q),
    bfs(Q, Path).
    
bfs2(Path) :-
    start(V),
    empty2(Q),
    put2(V, Q, Queue),
    bfs2(Queue, [], Path).
    
bfs2(Queue, _, []) :-
    is_empty2(Queue), !.
bfs2(Queue, Visited, [V|Path]) :-
    get2(Queue, V, Qu),
    \+ member(V, Visited), !,
    addall2(V1, e(V, V1), Qu, Q),
    bfs2(Q, [V|Visited], Path).
bfs2(Queue, Visited, [V|Path]) :-
    get2(Queue, V, Qu),
    member(V, Visited), !,
    bfs2(Qu, Visited, Path).
    
    
% zadanie 5

insert(X, leaf, node(leaf, X, leaf)) :- !. 
insert(X, node(L, X, R), node(L, X, R)) :- !.
insert(X, node(L, N, R), node(NL, N, R)) :- 
    X < N, 
    insert(X, L, NL). 
insert(X, node(L, N, R), node(L, N, NR)) :- 
    X > N, 
    insert(X, R, NR). 

find(X, node(_, X, _)) :- !. 
find(X, node(L, _, R)) :- 
    (find(X, L); 
    find(X, R)). 

findMax(node(_, X, leaf), X) :- !. 
findMax(node(_, _, R), X) :- 
    findMax(R, X). 

delMax(node(L, X, leaf), X, L) :- !. 
delMax(node(L, N, R), X, node(L, N, NR)) :- 
    delMax(R, X, NR). 

delete(X, node(leaf, X, R), R) :- !. 
delete(X, node(L, X, R), node(Rest, Max, R)) :- 
    delMax(L, Max, Rest), !. 
delete(X, node(L, N, R), node(NL, N, R)) :- 
    X < N, 
    delete(X, L, NL).
delete(X, node(L, N, R), node(L, N, NR)) :- 
    X > N,
    delete(X, R, NR).

empty(leaf).
    

% zadanie 6

concat([], A, A).
concat([H|T], A, R) :-
    A2 is 10 * A + H,
    concat(T, A2, R).
concat(D, N) :-
    concat(D, 0, N).
    
decimal(0, A, A).
decimal(N, A, L) :-
    N > 0,
    M is N // 10,
    R is N mod 10,
    decimal(M, [R|A], L).
decimal(N, L) :-
    decimal(N, [], L).

fill_aux([], []). 
fill_aux([c|T], [N|S]) :- 
    member(N, [1,3,5,7,9]), 
    fill_aux(T, S). 
fill_aux([s|T], [N|S]) :- 
    member(N, [0,2,4,6,8]), 
    fill_aux(T, S). 
fill(C, [L|T]) :- 
    fill_aux(C, [L|T]), 
    \+ L = 0. 
 
check([], [], _, []).
check([C|Cs], [N|Ns], M, [Res|Sum]) :-
    R is N * M,
    decimal(R, Res),
    fill(C, Res),
    check(Cs, Ns, M, Sum).

solve([C1,C2|Adds], Sum, Res) :- 
    reverse(Adds, [Result|RAdds]), 
    fill(C1, L1),
    concat(L1, N1),
    fill(C2, L2),
    concat(L2, N2),
    R is N1 * N2,
    decimal(R, Res),
    fill(Result, Res),
    check(RAdds, L2, N1, S),
    reverse(S, Sum).
    