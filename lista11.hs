-- zadanie 1

nat2 :: [(Integer,Integer)]
nat2 = [(x, y) | n <- [0..], x <- [0..n], y <- [0..n-x], x + y == n]

nat2' :: [(Integer,Integer)]
nat2' = [(x, n - x) | n <- [0..], x <- [0..n]]


-- zadanie 2

quicksort :: Ord a => [a] -> [a] 
quicksort [] = [] 
quicksort (m:xs) = quicksort [x | x <- xs, x < m] ++ [m] ++ quicksort [x | x <- xs, x >= m]


-- zadanie 3

merge :: Ord a => [a] -> [a] -> [a] 
merge xs [] = xs 
merge [] ys = ys 
merge xs@(x:xs') ys@(y:ys') 
    | x <= y = x : merge xs' ys 
    | otherwise = y : merge xs ys' 
 
split :: [a] -> ([a], [a])
split xs = split' xs xs where 
    split' xs [] = ([], xs) 
    split' xs [_] = ([], xs) 
    split' (x:xs) (_:_:ys) = 
        let (ls, rs) = split' xs ys 
        in (x:ls, rs) 

mergesort :: Ord a => [a] -> [a] 
mergesort []  = [] 
mergesort [x] = [x] 
mergesort xs  = merge (mergesort ls) (mergesort rs) 
    where (ls, rs) = split xs 
        
        
-- zadanie 4

merge' :: Ord a => [a] -> [a] -> [a] 
merge' xs@(x:xs') ys@(y:ys')
    | x < y = x : merge' xs' ys
    | x > y = y : merge' xs ys'
    | otherwise = x : merge' xs' ys'

d235 :: [Integer]
d235 = 1 : map (2*) d235 `merge'` map (3*) d235 `merge'` map (5*) d235


-- zadanie 5
          
mergesortn :: Ord a => Int -> [a] -> [a] 
mergesortn 0 _ = [] 
mergesortn 1 (x:_) = [x] 
mergesortn n xs = merge (mergesortn n2 xs) (mergesortn (n - n2) rs) 
    where n2 = n `div` 2
          rs = drop n2 xs
          
mergesort' :: Ord a => [a] -> [a] 
mergesort' xs = mergesortn (length xs) xs


-- zadanie 6

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving Show 

insert :: Ord a => a -> Tree a -> Tree a 
insert x Leaf = Node Leaf x Leaf 
insert x (Node ls n rs) 
    | x < n = Node (insert x ls) n rs 
    | otherwise = Node ls n (insert x rs) 

flatten :: Tree a -> [a] 
flatten Leaf = [] 
flatten (Node ls n rs) = flatten ls ++ [n] ++ flatten rs 

flatten' :: Tree a -> [a] 
flatten' tree = aux [] tree where 
    aux acc Leaf = acc
    aux acc (Node ls n rs) = aux (n : aux acc rs) ls  

treesort :: Ord a => [a] -> [a] 
treesort = flatten' . foldr insert Leaf 
          