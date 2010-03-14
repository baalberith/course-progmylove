% zadanie 2

man(socrates).
is_mortal(X) :- man(X).


% zadanie 3

parent(adam, helen).
parent(adam, ivonne).
parent(adam, anna).
parent(eve, helen).
parent(eve, ivonne).
parent(eve, anna).
parent(john, joshua).
parent(helen, joshua).
parent(ivonne, david).
parent(mark, david).

male(adam).
male(john).
male(mark).
male(joshua).
male(david).

female(eve).
female(helen).
female(ivonne).
female(anna).

sibling(X, Y) :- parent(Z, X), parent(Z, Y).
sister(X, Y) :- female(X), sibling(X, Y).
grandson(X, Y) :- male(X), parent(Z, X), parent(Y, Z).
cousin(X, Y) :- male(X), parent(Z1, X), parent(Z2, Y), sibling(Z1, Z2).
descendant(X, Y) :- parent(Y, X).
descendant(X, Y) :- parent(Z, X), descendant(Z, Y).
is_mother(X) :- female(X), parent(X, _).
is_father(X) :- male(X), parent(X, _).


% zadanie 4

connect(wroclaw, warszawa).
connect(wroclaw, krakow).
connect(wroclaw, szczecin).
connect(szczecin, lublin).
connect(szczecin, gniezno).
connect(warszawa, katowice).
connect(gniezno, gliwice).
connect(lublin, gliwice).

connection1(X, Y) :- connect(X, Z), connect(Z, Y), not(X=Y).

connection2(X, Y) :- connect(X, Y).
connection2(X, Y) :- connection1(X, Y).
connection2(X, Y) :- connect(X, Z), connection1(Z, Y).

connection(X, Y) :- connect(X, Y).
connection(X, Y) :- connect(X, Z), connection(Z, Y).

destination(X, X).
destination(X, Y) :- connect(X, Z), destination(Z, Y).


% zadanie 5

append([], X, X).
append([H|T], X, [H|Y]) :-
   append(T, X, Y).


% zadanie 6

select(H, [H|T], T).
select(X, [H|T], [H|S]) :-
   select(X, T, S).
