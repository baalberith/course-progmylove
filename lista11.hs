-- zadanie 1

nat2 :: [(Integer,Integer)]
nat2 = [(x, y) | n <- [0..], x <- [0..n], y <- [0..n], x + y == n]

nat2' :: [(Integer,Integer)]
nat2' = [(x, n-x) | n <- [0..], x <- [0..n]]


-- zadanie 2

quicksort :: Ord a => [a] -> [a] 
quicksort [] = [] 
quicksort (m:xs) = quicksort [x | x <- xs, x < m] ++ [m] ++ quicksort [x | x <- xs, x >= m]


-- zadanie 3

merge :: Ord a => [a] -> [a] -> [a] 
merge xs [] = xs 
merge [] ys = ys 
merge xs@(x:xs') ys@(y:ys') 
    | x <= y    = x : merge xs' ys 
    | otherwise = y : merge xs ys' 
 
split :: [a] -> ([a], [a])
split lst = split' lst lst where 
    split' xs [] = ([], xs) 
    split' xs [_] = ([], xs) 
    split' (x:xs) (_:_:ys) = 
        let (a,b) = split' xs ys 
        in (x:a, b) 

mergesort :: Ord a => [a] -> [a] 
mergesort []  = [] 
mergesort [x] = [x] 
mergesort xs  = merge (mergesort ls) (mergesort gs) 
    where (ls, gs) = split xs 
        
        
-- zadanie 4

merge' :: Ord a => [a] -> [a] -> [a] 
merge' xs@(x:xs') ys@(y:ys')
    | x < y = x : merge' xs' ys
    | x > y = y : merge' xs ys'
    | otherwise = x : merge' xs' ys'

d235 :: [Integer]
d235 = 1 : map (2*) d235 `merge'` map (3*) d235 `merge'` map (5*) d235


-- zadanie 5

mergen m xs 0 _ = take m xs
mergen 0 _ n ys = take n ys
mergen m xs@(x:xs') n ys@(y:ys') 
    | x <= y    = x : mergen (m - 1) xs' n ys 
    | otherwise = y : mergen m xs (n - 1) ys' 

msortn :: Ord a => Integer -> [a] -> [a] 
msortn 0 _ = []
msortn 1 (x:_) = [x] 
msortn n xs = mergen (fromInteger m) (msortn m xs) (fromInteger (n - m)) (msortn (n - m) gs) 
    where m = n `div` 2
          gs = drop (fromInteger m) xs 
          
          
-- zadanie 6

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving Show 

insert :: Ord a => a -> Tree a -> Tree a 
insert x Leaf = Node Leaf x Leaf 
insert x (Node l y r) 
    | x < y     = Node (insert x l) y r 
    | otherwise = Node l y (insert x r) 

flatten :: Tree a -> [a] 
flatten Leaf = [] 
flatten (Node l x r) = flatten l ++ [x] ++ flatten r 

treesort :: Ord a => [a] -> [a] 
treesort = flatten . foldr insert Leaf 
          