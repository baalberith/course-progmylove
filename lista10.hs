import List
import Char
   
   
-- zadanie 1

roots :: (Double, Double, Double) -> [Double] 
roots (a, b, c) 
    | a == 0 = if b /= 0 then [ -c / b ] else []
    | delta > 0 = [ (-b - sqrt delta) / (2 * a), (-b + sqrt delta) / (2 * a) ] 
    | delta == 0 = [ -b / (2 * a) ] 
    | otherwise  = [] 
    where delta = b * b - 4 * a * c 
          
roots' :: (Double, Double, Double) -> [Double] 
roots' (a, b, c) 
    | a /= 0 =
        let delta = b * b - 4 * a * c in
        case delta `compare` 0.0 of 
            GT -> [ (-b - sqrt delta) / (2 * a), (-b + sqrt delta) / (2 * a) ] 
            EQ -> [ -b / (2 * a) ] 
            LT -> [] 
    | b /= 0 = [ -c / b ] 
    | otherwise  = [] 
    

-- zadanie 2

data Roots = No | One Double | Two (Double, Double) deriving Show 

roots2 :: (Double, Double, Double) -> Roots 
roots2 (a, b, c) 
    | a == 0 = if b /= 0 then One (-c / b) else No
    | delta > 0 = Two ((-b - sqrt(delta)) / (2 * a), (-b + sqrt(delta)) / (2 * a))
    | delta == 0 = One (-b / (2 * a))
    | otherwise  = No
    where delta = b * b - 4 * a * c 

roots2' :: (Double, Double, Double) -> Roots 
roots2' (a, b, c) 
    | a /= 0 =
        let delta = b * b - 4 * a * c in
        case delta `compare` 0.0 of 
            GT -> Two ((-b - sqrt delta) / (2 * a), (-b + sqrt delta) / (2 * a))
            EQ -> One (-b / (2 * a))
            LT -> No
    | b /= 0 = One (-c / b)
    | otherwise  = No
    
    
-- zadanie 3

integerToString :: Integer -> String 
integerToString n 
    | n < 0 = "-" ++ integerToString (-n)
    | n == 0 = "0"
    | otherwise = reverse $ unfoldr f n where
        f m          
            | m > 0 = Just (intToDigit $ fromEnum r, q) 
            | otherwise = Nothing 
            where (q, r) = m `quotRem` 10 
                  
                  
-- zadanie 4

newtype FSet a = FSet (a -> Bool) 

fEmpty :: Ord a => FSet a 
fEmpty = FSet (\_ -> False) 

fSingleton :: Ord a => a -> FSet a
fSingleton a = FSet (== a) 

fInsert :: Ord a => a -> FSet a -> FSet a 
fInsert a set = fUnion (fSingleton a) set 

fFromList :: Ord a => [a] -> FSet a 
fFromList as = foldr fInsert fEmpty as

fUnion :: Ord a => FSet a -> FSet a -> FSet a 
fUnion (FSet f) (FSet g) = FSet (\a -> f a || g a) 

fIntersection :: Ord a => FSet a -> FSet a -> FSet a 
fIntersection (FSet f) (FSet g) = FSet (\a -> f a && g a) 

fMember :: Ord a => a -> FSet a -> Bool 
fMember a (FSet f) = f a 


-- zadanie 5

ins :: Ord a => a -> [a] -> [a] 
ins x [] = [x] 
ins x (y:ys) 
    | x <= y = x : y : ys 
    | otherwise = y : (ins x ys) 

insSort :: Ord a => [a] -> [a] 
insSort [] = [] 
insSort (x:xs) = ins x (insSort xs) 


-- zadanie 6

del :: Ord a => a -> [a] -> [a] 
del x [] = [] 
del x (y:ys) 
    | x == y = ys 
    | otherwise = y : del x ys 

selSort :: Ord a => [a] -> [a] 
selSort [] = [] 
selSort (xs) = x : selSort (del x xs) 
    where x = minimum xs 
