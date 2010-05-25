-- zadanie 1

-- 1, 2 :: (Num t) => t
-- (*) :: (Num a) => a -> a -> a
-- sin :: (Floating a) => a -> a
-- map :: (a -> b) -> [a] -> [b]

f1 :: (Num (t -> (a -> b) -> [a] -> [b]), Num ((a -> b) -> [a] -> [b])) => t -> (a -> b) -> [a] -> [b]
f1 x = map -1 x

f2 :: (Num (a -> b)) => [a] -> [b]
f2 x = map (-1) x

f3 :: (Num [t]) => t -> [[t]]
f3 x = [x] : [1]

f4 :: (Floating b, Num (a -> b)) => (a -> b) -> a -> b
f4 x = x * sin . 1


-- zadanie 2

-- (f . g) x = f (g x)
-- f $ x = f x
-- flip f a b = f b a

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b
-- flip :: (a -> b -> c) -> b -> a -> c

g1 :: (a1 -> b -> c) -> a1 -> (a -> b) -> a -> c
g1 = (.) (.)

g2 :: (a1 -> a -> b) -> a1 -> a -> b
g2 = (.) ($)

g3 :: (b -> c) -> (a -> b) -> a -> c
g3 = ($) (.)

g4 :: b -> (a -> b -> c) -> a -> c
g4 = flip flip

g5 :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
g5 = (.) (.) (.)

g6 :: (b -> c) -> (a -> b) -> a -> c
g6 = (.) ($) (.)

g7 :: (a -> a1 -> b) -> a -> a1 -> b
g7 = ($) (.) ($)

g8 :: (a -> ((a1 -> b -> c1) -> b -> a1 -> c1) -> c) -> a -> c
g8 = flip flip flip

g9 :: [[Char]]
g9 = tail $ map tail [[], ['a']]

g10 :: t
g10 = let x = x in x x
          
g11 :: Char
g11 = (\_ -> 'a') (head [])

g12 :: Char
g12 = (\(_,_) -> 'a') (head [])


-- zadanie 3

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ c []   = c
-- foldr (*) c (x:xs) = x * foldr (*) c xs
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl _ c []   = c
-- foldl (*) c (x:xs) = foldl (*) (c * x) xs
-- const :: a -> b -> a
-- flip :: (a -> b -> c) -> (b -> a -> c)
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

length1 = foldr (flip $ const . succ) 0

length2 = foldl (const . succ) 0

(+++) = flip $ foldr (:)

concat' = foldr (+++) []

reverse' = foldl (flip (:)) []

sum' = foldl (+) 0


-- zadanie 4

class Set a where
    empty :: a
    isEmpty :: a -> Bool
    singleton :: Integer -> a
    fromList :: [Integer] -> a
    union :: a -> a -> a
    intersection :: a -> a -> a
    member :: Integer -> a -> Bool


newtype ListSet = ListSet [Integer]

instance Set ListSet where
    empty = ListSet []
    isEmpty (ListSet []) = True
    isEmpty (ListSet _) = False
    singleton x = ListSet [x]
    fromList xs = ListSet xs
    union (ListSet xs) (ListSet ys) = 
        ListSet $ foldr (\x acc -> if x `elem` acc then acc else x : acc) ys xs
    intersection (ListSet xs) (ListSet ys) = 
        ListSet $ foldr (\x acc -> if x `elem` xs then x : acc else acc) [] ys
    member x (ListSet xs) = x `elem` xs
    
    
data TreeSet = Node TreeSet Integer TreeSet | Leaf

insert' :: Integer -> TreeSet -> TreeSet
insert' x Leaf = Node Leaf x Leaf 
insert' x (Node ls n rs) 
    | x < n = Node (insert' x ls) n rs 
    | x > n = Node ls n (insert' x rs) 
    | otherwise = Node ls n rs
    
flatten' :: TreeSet -> [Integer] 
flatten' tree = aux [] tree where 
    aux acc Leaf = acc
    aux acc (Node ls n rs) = aux (n : aux acc rs) ls  
            
instance Set TreeSet where
    empty = Leaf
    isEmpty Leaf = True
    isEmpty (Node _ _ _) = False
    singleton x = Node Leaf x Leaf
    fromList xs = foldr insert' empty xs
    union xs ys = foldr insert' ys (flatten' xs)
    intersection xs ys = 
        fromList [x | x <- flatten' xs, member x ys] 
    member _ Leaf = False 
    member x (Node ls n rs) 
        | x == n = True 
        | otherwise = member x ls || member x rs

    
-- zadanie 5

class Monoid a where
   (***) :: a -> a -> a
   e :: a
   
infixl 6 ***


-- (^^^) :: Monoid a => a -> Integer -> a
-- a ^^^ 0 = e
-- a ^^^ (n + 1) = a *** a ^^^ n

-- iter :: (a -> a) -> a -> Integer -> a
-- iter f x n = iter' f n $ x where 
--     iter' f 0 = id
--     iter' f n = f . (iter' f $ n - 1)

-- (^^^) :: Monoid a => a -> Integer -> a
-- x ^^^ n = iter (*** x) e n

(^^^) :: Monoid a => a -> Integer -> a
x ^^^ 0 = e
x ^^^ n 
    | n < 0 = undefined
    | even n = y *** y
    | otherwise = y *** y *** x
    where y = x ^^^ (n `div` 2)

infixr 7 ^^^


newtype Z9876543210 = Z9876543210 Integer

instance Monoid Z9876543210 where
    Z9876543210 a *** Z9876543210 b = Z9876543210 $ a * b `mod` 9876543210
    e = Z9876543210 1  
  
res1 = let Z9876543210 x = Z9876543210 123456789 ^^^ 1234567890 in x


-- zadanie 6

data Mtx2x2 a = Mtx2x2 a a a a

instance Num a => Monoid (Mtx2x2 a) where
  (Mtx2x2 a b c d) *** (Mtx2x2 e f g h) = Mtx2x2 (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)
  e = Mtx2x2 1 0 0 1
  
-- fib :: Num a => Integer -> a
-- fib = aux 1 1 where
--     aux f1 f2 n =
--         case n `compare` 0 of
--             LT -> 0
--             EQ -> f1
--             GT -> aux f2 (f1 + f2) (n - 1)

fib :: (Num a) => Integer -> a
fib n = f where (Mtx2x2 _ _ _ f) = (Mtx2x2 0 1 1 1) ^^^ (n - 1)
    
res2 = fib 123456789
