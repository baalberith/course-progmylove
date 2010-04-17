letters(MaxL, Letters) :-
    letters(MaxL, [], Letters).
letters('A', A, ['A'|A]) :- !.
letters(L, A, Letters) :-
    char_code(L, N),
    N1 is N - 1,
    char_code(L1, N1), 
    letters(L1, [L|A], Letters).
    
    
board(M, N, Board) :-
    findall(Cell,
        (between(1, N, Y),
        between(1, M, X),
        Cell = cell((X, Y), _, _, _)),
        Board),
    maplist(tie_neighbors(Board), Board).
    
tie_neighbors(Board, cell(XY, RNbs, CNbs, _)) :-
    include(row_neighbor(XY), Board, RNbs),
    include(col_neighbor(XY), Board, CNbs).
    
row_neighbor((X, Y), cell((U, V), _, _, _)) :-
    Y = V, X \= U.
col_neighbor((X, Y), cell((U, V), _, _, _)) :-
    X = U, Y \= V.
    
   
fill(Input, Board) :-
    maplist(fill_board(Input), Board).
    
fill_board(Input, cell((X, Y), _, _, C)) :-
    member((X, Y, C), Input), !.
fill_board(_, _).


dfs([], _, _, _) :- !.
dfs([Cell|Cells], Letters, M, N) :-
    cell(_, RNbs, CNbs, Letter) = Cell,
    var(Letter), !,
    member(Letter, Letters),
    dfs2(RNbs, Letters),
    check(RNbs, Letter, M),
    dfs2(CNbs, Letters),
    check(CNbs, Letter, N),
    dfs(Cells, Letters, M, N).
dfs([_|Cells], Letters, M, N) :-
    dfs(Cells, Letters, M, N).
    
dfs2([], _) :- !.
dfs2([Cell|Cells], Letters) :-
    cell(_, _, _, Letter) = Cell,
    var(Letter), !,
    member(Letter, Letters),
    dfs2(Cells, Letters).
dfs2([_|Cells], Letters) :-
    dfs2(Cells, Letters).
    
check(Nbs, Letter, N) :-
    extract(Nbs, Letters),
    count(Letter, Letters, C),
    C1 is C + 1,
    N mod C1 =:= 0,
    check_aux(Letters, [Letter|Letters], C1).

check_aux([], _, _).
check_aux([Letter|Rest], Letters, C) :-
    count(Letter, Letters, C),
    check_aux(Rest, Letters, C).
    
count(C, L, N) :-
    count(L, C, 0, N).
count([], _, A, A).
count([H|T], C, A, N) :-
    H = C, !,
    A1 is A + 1,
    count(T, C, A1, N).
count([_|T], C, A, N) :-
    count(T, C, A, N).
    
    
proper([Cell|_]) :-
    cell(_, RNbs, CNbs, Letter) = Cell,
    extract(RNbs, RLetters),
    extract(CNbs, CLetters),
    (same_letters(RLetters, Letter);
    same_letters(CLetters, Letter)), !.
proper([_|Cells]) :-
    proper(Cells).
    
same_letters([], _).
same_letters([Letter|Rest], Letter) :-
    same_letters(Rest, Letter).
    
    
extract(Board, Solution) :-
    maplist(letter, Board, Solution).
    
letter(cell(_, _, _, Letter), Letter). 


solve(M, N, MaxL, Input, Solution) :-
    letters(MaxL, Letters),
    board(M, N, Board), 
    fill(Input, Board),
    dfs(Board, Letters, M, N),
    proper(Board),
    extract(Board, Sol),
    Solution = Sol.    


write_solution(Solution, M) :-
    length(Solution, Len),
    N is Len - 1,
    write_solution(Solution, N, M).
    
write_solution([], _, _) :- 
    !, nl.
write_solution([H|T], N, M) :-
    N mod M =:= 0, !,
    write(H), nl,
    N1 is N - 1,
    write_solution(T, N1, M).
write_solution([H|T], N, M) :-
    write(H), write(' '),
    N1 is N - 1,
    write_solution(T, N1, M).


main(File) :-
    open(File, read, _, [alias(params)]),
    read(params, M),
    read(params, N),
    read(params, MaxC),
    read(params, Input),
    close(params),
    solve(M, N, MaxC, Input, Solution),
    write_solution(Solution, M), 
    fail.
    