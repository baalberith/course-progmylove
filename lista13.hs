import Control.Monad
import Data.Maybe
   
   
-- zadanie 1

insert1 :: a -> [a] -> [[a]]
insert1 x [] = [[x]] 
insert1 x ys@(y:ys') = (x:ys) : map (y:) (insert1 x ys')

permi1:: [a] -> [[a]] 
permi1 [] = [[]] 
permi1 (x:xs) = concatMap (insert1 x) (permi1 xs)


insert2 :: a -> [a] -> [[a]]
insert2 x [] = [[x]] 
insert2 x ys@(y:ys') = (x:ys) : [ y:zs | zs <- insert2 x ys' ]

permi2:: [a] -> [[a]] 
permi2 [] = [[]] 
permi2 (x:xs) = [ p | perm <- permi2 xs, p <- insert2 x perm ]
    
    
insert3 :: a -> [a] -> [[a]]
insert3 x [] = return [x] 
insert3 x ys@(y:ys') = 
    return (x:ys) `mplus` do 
        zs <- insert3 x ys'
        return (y:zs) 
    
permi3:: [a] -> [[a]] 
permi3 [] = return [] 
permi3 (x:xs) = do 
    perm <- permi3 xs 
    insert3 x perm 
    

-- zadanie 2


select1 :: [a] -> [(a, [a])] 
select1 [x] = [(x, [])] 
select1 (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select1 xs)

perms1 :: [a] -> [[a]] 
perms1 [] = [[]] 
perms1 xs = concatMap (\(y, ys) -> map (y:) (perms1 ys)) (select1 xs) 


select2 :: [a] -> [(a, [a])] 
select2 [x] = [(x, [])] 
select2 (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- select2 xs ]

perms2::[a] -> [[a]] 
perms2 [] = [[]] 
perms2 xs = [ y:perm | (y, ys) <- select2 xs, perm <- perms2 ys ] 


select3 :: [a] -> [(a, [a])] 
select3 [x] = return (x, []) 
select3 (x:xs) = 
    return (x, xs) `mplus` do 
        (y, ys) <- select3 xs 
        return (y, x:ys) 
    
perms3:: [a] -> [[a]] 
perms3 [] = return []
perms3 xs = do 
    (y, ys) <- select3 xs 
    perm <- perms3 ys 
    return (y:perm) 
      
      
-- zadanie 3

sublists1:: [a] -> [[a]] 
sublists1 [] = [[]] 
sublists1 (x:xs) = map (x:) subs ++ subs 
    where subs = sublists1 xs
          
sublists1':: [a] -> [[a]] 
sublists1' [] = [[]] 
sublists1' (x:xs) = foldr (\sub sublists -> (x:sub) : sublists) subs subs
    where subs = sublists1' xs


sublists2:: [a] -> [[a]]
sublists2 [] = [[]] 
sublists2 (x:xs) = [ s | sub <- sublists2 xs, s <- [x:sub, sub] ]


sublists3:: [a]-> [[a]] 
sublists3 [] = return [] 
sublists3 (x:xs) = do 
    sub <- sublists3 xs 
    return (x:sub) `mplus` 
        return sub 


-- zadanie 4

prod1 :: [Integer] -> Integer
prod1 xs = 
    case prod' xs of
        Nothing -> 0
        Just n -> n
    where prod' = foldM (\acc n -> if n == 0 then Nothing else Just $ n * acc) 1
          
          
prod2 :: [Integer] -> Integer   
prod2 = fromMaybe 0 . foldM (\x y -> guard (y /= 0) >> return (x * y)) 1


prod3 :: [Integer] -> Integer
prod3 [] = undefined
prod3 xs = 
    (case result of 
        Nothing -> 0 
        Just n  -> n) where 
            result = prod' (Just 1) xs 
            prod' = foldl (\acc n  -> 
                case acc of 
                    Nothing -> acc 
                    Just 0  -> Nothing 
                    Just a  -> Just $ a * n)


prod4 :: [Integer] -> Integer
prod4 [] = undefined                   
prod4 xs = fromMaybe 0 result where 
    result = foldl aux (Just 1) xs 
    aux acc n = do 
        a <- acc
        guard (n /= 0) 
        return $ a * n


-- zadanie 5

queens :: Int -> [[Int]]
queens n = place n [1..n] [] []

place :: Int -> [Int] -> [Int] -> [Int] -> [[Int]]
place 0 _ _ _ = return []
place col rows diag1 diag2 = do
    (row, rows') <- select rows
    let d1 = row - col
    guard (d1 `notElem` diag1)
    let d2 = row + col
    guard (d2 `notElem` diag2)
    rest <- place (col-1) rows' (d1:diag1) (d2:diag2)
    return (row:rest)
  
select :: [a] -> [(a, [a])]
select [x] = return (x, []) 
select (x:xs) = 
    return (x, xs) `mplus` do 
        (y, ys) <- select xs 
        return (y, x:ys) 


hetmani :: Int -> [[Int]]
hetmani n = aux n n [] where 
    aux _ 0 board = return board 
    aux size col board = do 
        row <- [1..size] 
        guard $ nieszachuje row board 
        aux size (col-1) (row:board) 
              
nieszachuje :: Int -> [Int] -> Bool 
nieszachuje row board = aux 1 board where 
    aux _ [] = True 
    aux n (y:ys) = 
        y /= row && 
        y + n /= row && 
        y - n /= row && 
        aux (n+1) ys 
              
              
-- zadanie 6

-- 1) kończy się
-- 2) pętli się w nieskończoność
-- 3) zużywa nieskończoną ilość pamięci na stercie lub stosie

loop :: a
loop = loop

ones :: [Integer]
ones = 1 : ones

f1 = head $ 1 : loop -- 1)
f2 = fst (1, loop) -- 1)
f3 = length [loop, loop, loop] -- 1)
f4 = length ones -- 2)
f5 = sum ones -- 3)
f6 = last ones -- 2)
f7 = last [1..] -- 3)
f8 = let f [] = 0; f (_:xs) = 2 + f xs in f ones -- 3)
