import Data.List
import Data.Char
   
   
-- zadanie 2

fibSum :: Integer
fibSum = fibSum' 1 1 0 where
    limit = 4 * 10 ^ 6
    fibSum' fn fn1 acc
        | fn > limit = acc
        | otherwise = fibSum' fn1 (fn + fn1) (if even fn then acc + fn else acc)

fibSum' = sum $ filter even $ takeWhile (<= 4 * 10 ^ 6) fib where
    fib = 1 : 1 : zipWith (+) fib (tail fib)
    

-- zadanie 3

maxSeqLen :: Integer
maxSeqLen = maxSeqLen' limit where 
    limit = 10 ^ 6
    seqLen 1 = 1
    seqLen n
        | even n = 1 + seqLen (n `div` 2)
        | otherwise = 1 + seqLen (3 * n + 1)
    maxSeqLen' 0 = 0
    maxSeqLen' n
        | len > maxLen = len
        | otherwise = maxLen
        where len = seqLen n
              maxLen = maxSeqLen' (n - 1)
              
maxSeqLen' = maximum $ map (length . seq) [1..10^6] where
    seq 1 = []
    seq n = n : seq (if even n then n `div` 2 else 3 * n + 1)
                     
maxSeqLen'' = maximum $ map (length . unfoldr (\n -> if n == 1 then Nothing else Just (n, if even n then n `div` 2 else 3 * n + 1))) [1..10^4]


-- zadanie 4

digSum :: Integer
digSum = digSum' (fact number) where 
    number = 100
    fact 0 = 1
    fact n = n * fact (n - 1)
    digSum' n 
        | n < 10 = n
        | otherwise = (n `mod` 10) + digSum' (n `div` 10)
        
digSum' = sum . unfoldr (\n -> if n == 0 then Nothing else Just (n `mod` 10, n `div` 10)) $ product [1..100]
                                  
digSum'' = sum . map (\c -> ord c - ord '0') . show $ product [1..100]
    
 
-- zadanie 5

iter :: (a -> a) -> a -> Integer -> a
iter f x n = iter' f n $ x where 
    iter' _ 0 = id
    iter' f n = f . (iter' f (n - 1))
    
iter' f x n
    | n < 0 = undefined
    | n == 0 = x
    | otherwise = f $ iter f x (n - 1)
    

-- zadanie 6

fib :: Integer -> Integer
fib n = fib' 1 1 n where
    fib' acc1 _ 0 = acc1
    fib' _ acc2 1 = acc2
    fib' acc1 acc2 n = fib' acc2 (acc1 + acc2) (n - 1)
