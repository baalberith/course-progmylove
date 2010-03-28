gen_chars(MaxC, List) :-
    gen_chars(MaxC, [], List).
gen_chars('A', A, ['A'|A]) :- !.
gen_chars(C, A, L) :-
    char_code(C, N),
    N1 is N - 1,
    char_code(C1, N1), 
    gen_chars(C1, [C|A], L).
    
gen_row(N, M, Input, Chars, Row) :-
    gen_row(N, M, Input, Chars, [], Row).
gen_row(_, 0, _, _, A, A) :- !.
gen_row(N, M, Input, Chars, A, Row) :-
    member((M, N, C), Input), !,
    M1 is M - 1,
    gen_row(N, M1, Input, Chars, [C|A], Row).
gen_row(N, M, Input, Chars, A, Row) :-
    member(X, Chars),
    M1 is M - 1,
    gen_row(N, M1, Input, Chars, [X|A], Row).
    
gen_rows(N, M, Input, Chars, Rows) :-
    gen_rows(N, M, Input, Chars, [], Rows).
gen_rows(0, _, _, _, A, A) :- !.
gen_rows(N, M, Input, Chars, A, Rows) :-
    gen_row(N, M, Input, Chars, Row),
    N1 is N - 1,
    gen_rows(N1, M, Input, Chars, [Row|A], Rows).

count(C, L, N) :-
    count(L, C, 0, N).
count([], _, A, A).
count([H|T], C, A, N) :-
    H = C, !,
    A1 is A + 1,
    count(T, C, A1, N).
count([_|T], C, A, N) :-
    count(T, C, A, N).
    
head(H, [H|_]).
tail(T, [_|T]).
    
proper(L) :-
    head(H, L),
    count(H, L, N),
    tail(T, L),
    proper(T, L, N).

proper([], _, _).
proper([H|T], L, N) :-
    count(H, L, N1),
    N1 = N,
    proper(T, L, N1).

proper_count([]).
proper_count([H|T]) :-
    proper(H),
    proper_count(T).
    
transpose_rows([Word], Cs) :- !,
        list2columns(Word, Cs).
transpose_rows([Word|Words], Cs) :- !,
        transpose_rows(Words, Cs0),
        put_columns(Word, Cs0, Cs).
        
list2columns([], []).
list2columns([X|Xs], [[X]|Zs]) :- 
    list2columns(Xs, Zs).

put_columns([], Cs, Cs).
put_columns([X|Xs], [C|Cs0], [[X|C]|Cs]) :- 
    put_columns(Xs, Cs0, Cs).
    
same([H|T]) :-
    same(T, H).

same([], _).
same([H|T], N) :-
    H = N,
    same(T, N).
    
same_letters([H|T]) :- 
    (same(H) -> true;
    same_letters(T)).

solve(M, N, MaxC, Input, Solution) :-
    gen_chars(MaxC, Chars),
    gen_rows(N, M, Input, Chars, Rows),
    proper_count(Rows),
    transpose_rows(Rows, Cols),
    proper_count(Cols),
    (same_letters(Rows);
    same_letters(Cols)),
    Solution = Rows.
   
write_solution([]).
write_solution([H|T]) :-
    write(H), nl,
    write_solution(T).
    
main(File) :-
    open(File, read, _, [alias(params)]),
    read(params, M),
    read(params, N),
    read(params, MaxC),
    read(params, Input),
    close(params),
    solve(M, N, MaxC, Input, Solution),
    write_solution(Solution), nl, fail.
