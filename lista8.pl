% zadanie 0

digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

number(N) -->
    digit(D),
    number(N, D).
number(N, A) -->
    digit(D), !,
    { A1 is 10 * A + D },
    number(N, A1).
number(N, N) --> "".

expr(N) --> 
    number(A), 
    expr(N, A).
expr(N, A) -->
    "-", !,
    number(N1),
    { A1 is A - N1 },
    expr(N, A1).
expr(N, N) --> "".

% prawostronna łączność
expr1(N) --> number(N).
expr1(N) --> number(N1), "-", expr1(N2),
    { N is N1 - N2 }.
    
% zapętla się po nawrocie
expr2(N) --> number(N).
expr2(N) --> expr2(N1), "-", number(N2),
    { N is N1 - N2 }.


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
