select(H, [H|T], T).
select(X, [H|T], [H|S]) :-
   select(X, T, S).

cols(N, N, [N]) :- !. 
cols(N, M, [N|Rest]) :- 
    N1 is N + 1, 
    cols(N1, M, Rest). 

queens(N, Res) :-
    cols(1, N, Cols),
    place(N, Cols, [], [], Res).

place(0, _, _, _, []) :- !.
place(Col, Rows, Diag1, Diag2, [Row|Rest]) :-
    Col > 0,
    select(Row, Rows, Rows1),
    D1 is Row - Col,
    \+ member(D1, Diag1),
    D2 is Row + Col,
    \+ member(D2, Diag2),
    Col1 is Col - 1,
    place(Col1, Rows1, [D1|Diag1], [D2|Diag2], Rest).