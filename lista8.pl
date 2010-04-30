% zadanie 3

'!'(N, M) :-
    fact1(N, 1, M).
fact1(0, A, A) :- !.
fact1(N, A, M) :-
    N > 0,
    A1 is A * N,
    N1 is N - 1,
    fact1(N1, A1, M).

'!!'(N, M) :-
    N mod 2 =:= 1, !,
    fact2(N, 1, M).
'!!'(N, M) :-
    N > 1,
    N1 is N - 1,
    fact2(N1, 1, M).
fact2(1, A, A) :- !.
fact2(N, A, M) :-
    N > 2,
    A1 is A * N,
    N1 is N - 2,
    fact2(N1, A1, M).
    
:- arithmetic_function('!'/1).
:- arithmetic_function('!!'/1).

:- op(300, xf, '!').
:- op(300, xf, '!!').


% zadanie 4
 
lang1 --> "".
lang1 --> "a", lang1, "b".

% phrase(lang1, "").     % 0
% phrase(lang1, "aabb"). % 2
% phrase(lang1, "aba").  % 2

% expand_term((lang1 --> ""), X).
% expand_term((lang1 --> "a", lang1, "b"), X).

lang2(N) --> "a", !, lang2(M), "b",
    { N is M + 1 }.
lang2(0) --> "".

s(N) --> a(N, 0).
a(N, A) --> "a", !,
    { A1 is A + 1 },
    a(N, A1).
a(A, A) --> b(A).
b(N) --> "b", !,
    { N1 is N - 1 },
    b(N1).
b(0) --> "".


% zadanie 5

tree1 --> "*".
tree1 --> "(", tree1, tree1, ")".

tree2(node(T1, T2)) --> "(", !, tree2(T1), tree2(T2), ")".
tree2(leaf) --> "*".

t(Abs) --> 
    t(Abs, []). 
t(Abs, A) --> 
    "(", !, 
    t(Abs, ['(' | A]). 
t(Abs, A) --> 
    "*", !, 
    t(Abs, [leaf | A]).   
t(Abs, [T2, T1, '(' | A]) --> 
    ")", !, 
    t(Abs, [node(T1, T2) | A]). 
t(Abs, [Abs]) --> "". 


% zadanie 6

simple_expression(a) --> "a".
simple_expression(b) --> "b".
simple_expression(E) --> "(", expression(E), ")".

expression(N) --> simple_expression(A), expression(N, A).
expression(N, A) --> "*", !, simple_expression(N1), 
    { A1 = A * N1 }, expression(N, A1).
expression(N, N) --> "".

symbol(a) --> "a".
symbol(b) --> "b".

e(Abs) --> 
    e(Abs, []). 
e(Abs, A) --> 
    symbol(S), !, 
    e(Abs, [S | A]). 
e(Abs, [E1, *, E2 | A]) --> 
    "*", !, 
    e(Abs, [*, E2 * E1 | A]). 
e(Abs, A) --> 
    "*", !, 
    e(Abs, [* | A]).        
e(Abs, A) --> 
    "(", !, 
    e(Abs, ['(' | A]).        
e(Abs, [E1, *, E2, '(' | A]) --> 
    ")", !, 
    e(Abs, [E2 * E1 | A]).        
e(Abs, [E, '(' | A]) --> 
    ")", !, 
    e(Abs, [E | A]). 
e(E2 * E1, [E1, *, E2]) --> !. 
e(Abs, [Abs]) --> "". 

variable(a) --> "a".
variable(b) --> "b".

ex(X) --> ex(X, []). 
ex(X, [variable(R), *, variable(L)|A]) --> !, ex(X, [variable(*(L, R))|A]). 
ex(X, A) --> "a", !, ex(X, [variable(a)|A]). 
ex(X, A) --> "b", !, ex(X, [variable(b)|A]). 
ex(X, A) --> "*", !, ex(X, [*|A]). 
ex(X, A) --> "(", !, ex(X, ['()'|A]). 
ex(X, [variable(E), '()'|A]) --> ")", !, ex(X, [variable(E)|A]). 
ex(X, [variable(X)]) --> "".
