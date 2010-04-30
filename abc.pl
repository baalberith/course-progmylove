% I**** K***
% zadanie 1 wersja 0
% łamigłówka A, B, C



% solve mając dane M, N - rozmiar planszy, InputList - listę liter początkowo znajdujących się na planszy, rozwiązuje łamigłówkę.

solve(M, N, MaxL, InputList, Solution) :-
    % generuje listę dostepnych liter.
    gen_letters(MaxL, Letters),
    % tworzy pustą planszę.
    make_board(M, N, Board), 
    % wypełnia planszę literami początkowo znajdującymi się na planszy.
    fill_board(Board, InputList),
    % wypełnia puste komórki planszy literami dbając o zachowanie 1 warunku rozwiązania.
    fill_cells(Board, Letters, M, N),
    % sprawdza zachowanie 1 warunku rozwiązania.
    check_board_1(Board, M, N),
    % sprawdza zachowanie 2 warunku rozwiązania.
    check_board_2(Board),
    % z planszy wyodrębnia rozwiązanie.
    extract_letters(Board, Solution).  
    
    

% gen_letters generuje listę liter z przediału 'A' - MaxL.

gen_letters(MaxL, Letters) :-
    gen_letters(MaxL, [], Letters).
gen_letters('A', A, ['A'|A]) :- !.
gen_letters(Letter, A, Letters) :-
    char_code(Letter, C),
    C1 is C - 1,
    char_code(PrevLetter, C1), 
    gen_letters(PrevLetter, [Letter|A], Letters).   


% make_board tworzy listę (palnszę M x N) komórek, których pierwszy element to współrzędne (X, Y) komórki, 
% drugi - lista reszty komórek w tym samym rzędzie, trzeci - lista reszty komórek w tej samej kolumnie, 
% czwarty - nieukonkretniona zmienna, w której znajdą się litery będące rozwiazaniem łamigłówki.

make_board(M, N, Board) :-
    findall(Cell,
        (between(1, N, Y), between(1, M, X),
        Cell = cell((X, Y), _, _, _)),
        Board),
    maplist(tie_neighbors(Board), Board).

% tie_neighbors ukonkretnia zmienne zawierające listę sąsiadów (komórek znajdujących się odpowiednio 
% w tym samym rzędzie / tej samej kolumnie) komórki o współrzędnych (X, Y).

tie_neighbors(Board, cell(XY, RowNeighbors, ColNeighbors, _)) :-
    include(row_neighbor(XY), Board, RowNeighbors),
    include(col_neighbor(XY), Board, ColNeighbors).

% row_neighbor sprawdza czy komórka o współrzędnych (U, V) znajduje się w tym samym rzędzie co komórka o współrzednych (X, Y).

row_neighbor((X, Y), cell((U, V), _, _, _)) :-
    Y = V, X \= U.
    
% col_neighbor sprawdza czy komórka o współrzędnych (U, V) znajduje się w tej samej kolumniee co komórka o współrzednych (X, Y).

col_neighbor((X, Y), cell((U, V), _, _, _)) :-
    X = U, Y \= V. 


% fill_board wypełnia planszę danymi wejściowymi (literami znajdującymisie już na planszy).

fill_board(Board, InputList) :-
    maplist(fill_board_aux(InputList), Board).
    
% fill_board_aux sprawdza, czy komórka o współrzędnych (X, Y) znajduje się na planszy początkowej. 
% jeśli tak, to wstawia ją na planszę.

fill_board_aux(InputList, cell((X, Y), _, _, CellLetter)) :-
    member((X, Y, CellLetter), InputList), !.
fill_board_aux(_, _).


% fill_cells wypełnia komórki na planszy literami z listy Letters, sprawdzając czy spełniają 1 warunek rozwiązania 
% (jeśli w jakimś rzędzie (kolumnie) są różne litery, to każda z nich występuje w tym rzędzie tyle samo razy).

fill_cells([], _, _, _) :- !.
fill_cells([Cell|Cells], Letters, M, N) :-
    cell(_, RowNeighbors, ColNeighbors, CellLetter) = Cell,
    var(CellLetter), !,
    member(CellLetter, Letters),
    fill_neighbors(RowNeighbors, Letters),
    check_1(RowNeighbors, CellLetter, M),
    fill_neighbors(ColNeighbors, Letters),
    check_1(ColNeighbors, CellLetter, N),
    fill_cells(Cells, Letters, M, N).
fill_cells([_|Cells], Letters, M, N) :-
    fill_cells(Cells, Letters, M, N).
    
% fill_neighbors wypełnia listę sąsiadów literami z listy Letters.

fill_neighbors([], _) :- !.
fill_neighbors([Cell|Cells], Letters) :-
    cell(_, _, _, CellLetter) = Cell,
    var(CellLetter), !,
    member(CellLetter, Letters),
    fill_neighbors(Cells, Letters).
fill_neighbors([_|Cells], Letters) :-
    fill_neighbors(Cells, Letters).

% check_1 sprawdza, czy zostaje zachowany 1 warunek rozwiązania dla poszczególnych rzędów / kolumn.

check_1(Neighbors, CellLetter, K) :-
    extract_letters(Neighbors, NeighborsLetters),
    count_letters(CellLetter, NeighborsLetters, C),
    C1 is C + 1,
    % sprawdza czy długość rzędu / kolumny K jest podzielna przez ilość liter CellLetter C1.
    K mod C1 =:= 0,
    % sprawdza czy ilość pozostałych liter NeighborsLetters jest taka sama.
    check_1_aux(NeighborsLetters, [CellLetter|NeighborsLetters], C1).

% check_1_aux sprawdza czy litera Letter występuje na liście Letters dokładnie C razy.

check_1_aux([], _, _) :- !.
check_1_aux([Letter|Rest], Letters, C) :-
    count_letters(Letter, Letters, C),
    check_1_aux(Rest, Letters, C).
    
% count_letters unkonkretnia C ilością liter Letter na liście Letters.

count_letters(Letter, Letters, C) :-
    count_letters(Letters, Letter, 0, C).
count_letters([], _, A, A) :- !.
count_letters([Letter|Letters], Letter, A, C) :- !,
    A1 is A + 1,
    count_letters(Letters, Letter, A1, C).
count_letters([_|Letters], Letter, A, C) :-
    count_letters(Letters, Letter, A, C).
    
    
% check_board_1 sprawdza czy cała wypełniona już plansza spełnia 1 warunek rozwiązania. 

check_board_1([], _, _) :- !.
check_board_1([Cell|Cells], M, N) :-
    cell(_, RowNeighbors, ColNeighbors, CellLetter) = Cell,
    check_1(RowNeighbors, CellLetter, M),
    check_1(ColNeighbors, CellLetter, N),
    check_board_1(Cells, M, N).
 

% check_board_2 sprawdza, czy zostaje zachowany 2 warunek rozwiązania 
% (przynajmniej w jednym rzędzie (kolumnie) wszystkie litery są takie same).

check_board_2([Cell|_]) :-
    cell(_, RowNeighbors, _, CellLetter) = Cell,
    extract_letters(RowNeighbors, RowLetters),
    check_board_2_aux(RowLetters, CellLetter), !.
check_board_2([Cell|_]) :-
    cell(_, _, ColNeighbors, CellLetter) = Cell,
    extract_letters(ColNeighbors, ColLetters),
    check_board_2_aux(ColLetters, CellLetter), !.
check_board_2([_|Cells]) :-
    check_board_2(Cells).

% check_board_2_aux sprawdza, czy wszystkie litery na liście są takie same.

check_board_2_aux([], _) :- !.
check_board_2_aux([Letter|Rest], Letter) :-
    check_board_2_aux(Rest, Letter).


% extract_letters wyodrębnia z listy komórek litery im odpowiadające.

extract_letters(List, Letters) :-
    maplist(cell_letter, List, Letters).
cell_letter(cell(_, _, _, CellLetter), CellLetter). 



% main wczytuje dane z pliku o nazwie File, rozwiązuje łamigłówkę i wypisuje jej rozwiązania.

main(File) :-
    open(File, read, _, [alias(params)]),
    read(params, M),
    read(params, N),
    read(params, MaxC),
    read(params, InputList),
    close(params),
    solve(M, N, MaxC, InputList, Solution),
    write_solution(Solution, M), 
    fail.
    
% write_solution wypisuje rozwiązanie w postaci ASCII-artu

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
    
