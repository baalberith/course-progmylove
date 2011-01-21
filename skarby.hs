-- Ilona Koda
-- zadanie 3 wersja 0
-- lamiglowka Skarby

import System.Environment 
import Data.List
import Control.Monad


data Task = Task {
    m :: Int,
    n :: Int,
    cells :: NumsBoard
}

type Cell = (Int, Int)
type Board = [Cell]

type NumsCell = (Int, Int, Int)
type NumsBoard = [NumsCell]

type Generator a = [a]
type State = (Board, Board, Int)


-- tworzy liste wspolrzednych reprezentujaca palnsze o rozmiarze m na n
board :: Int -> Int -> Board
board m n = [ (x, y) | x <- [1..m], y <- [1..n] ]

-- generuje wszystkich mozliwych sasiadow komorki o wspolrzednych (x, y)
neighbours :: Cell -> Generator Cell
neighbours (x, y) = [ (x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1] ]


coords :: NumsCell -> Cell
coords (x, y, _) = (x, y) 

number :: NumsCell -> Int
number (_, _, n) = n 


-- przeglada wszystkich sasiadow komorki z liczba skarbow
-- jesli dana komorka jest na liscie pustych to dodaje ja do listy skarbow lub nie
-- w przeciwnym wypadku sprawdza czy jest juz na liscie skarbow
treasures :: Board -> State -> Generator State
treasures [] state = return state
treasures (nb : nbs) state@(empt, tres, size) 
    | nb `elem` empt = do
        let empt' = delete nb empt
        treasures nbs (empt', tres, size) `mplus` do
            guard $ size > 0
            treasures nbs (empt', nb:tres, size-1)
    | nb `elem` tres = do
        guard $ size > 0
        treasures nbs (empt, tres, size-1)
    | otherwise = treasures nbs state

-- przeglada wszystkie komorki z liczba skarbow
-- probuje ustawic skarby w sasiedztwie danej komorki
-- sprawdza czy porzadana liczba skarbow zgadza sie z liczba ustawionych
solutions :: NumsBoard -> Board -> Board -> Generator Board
solutions [] _ tres = return $ tres
solutions _ [] [] = mzero
solutions (cell : cells) empt tres = do
    let nbs = neighbours $ coords cell
    (empt', tres', size) <- treasures nbs (empt, tres, number cell)
    guard $ size == 0
    solutions cells empt' tres'


task :: String -> Task
task str = Task (read m) (read n) (read cells) where
    lin = map init (lines str) 
    (m : n : cells : _) = lin 
    
solve :: Task -> Generator Board 
solve task = solutions (cells task) empt [] where 
    empt = board (m task) (n task) \\ map coords (cells task)


-- na podstawie listy z iloscia skarbow i listy skarbow tworzy plansze symboli
makeBoard :: Int -> Int -> NumsBoard -> Board -> [[String]]
makeBoard 0 _ _ _ = []    
makeBoard m n cells tres = reverse (makeRow n) : makeBoard (m-1) n cells tres where
    makeRow 0 = []
    makeRow n = char : makeRow (n-1) where
        char = if (m, n) `elem` tres then "*" else maybeNum m n
        maybeNum x y = foldl (\c (x', y', n) -> if (x, y) == (x', y') then show n else c) "_" cells

-- wypisuje liste rozwiazan danego zadania w postaci ASCII-artu
prettyPrint :: Task -> [Board] -> IO ()
prettyPrint tsk sols = printSols (m tsk) (n tsk) (cells tsk) sols where
    printSols _ _ _ [] = putStr "" 
    printSols m n cells (tres : sols) = do 
        printSol $ reverse (makeBoard m n cells tres)
        printSols n m cells sols
    printSol rows =
        mapM printRow rows >> putStrLn ""
    printRow row = do 
        mapM (\c -> putStr c >> putStr " ") row
        putStrLn ""
    

main :: IO () 
main = do 
    [fileName] <- getArgs 
    input <- readFile fileName 
    let tsk = task input
    let sols = solve tsk
    prettyPrint tsk sols
    