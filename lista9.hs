-- zadanie 2

fibSum :: Integer
fibSum = 
    let limit = 4 * 10 ^ 6
        fibSum' acc1 acc2 acc =
            if acc1 > limit 
                then acc
            else 
                if even acc1 
                    then fibSum' acc2 (acc1 + acc2) (acc + acc1)
                    else fibSum' acc2 (acc1 + acc2) acc   
    in fibSum' 1 1 0
    

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
                      

-- zadanie 4

digSum :: Integer
digSum = digSum' (fact number) where 
    number = 100
    fact 0 = 1
    fact n = n * fact (n - 1)
    digSum' 0 = 0
    digSum' n = (n `mod` 10) + digSum' (n `div` 10)
    
 
-- zadanie 5

iter :: (a -> a) -> a -> Integer -> a
iter f x n = iter' f n $ x where 
    iter' _ 0 = id
    iter' f n = f . (iter' f (n - 1))
    

-- zadanie 6

fib :: Integer -> Integer
fib n = fib' 1 1 n where
    fib' acc1 _ 0 = acc1
    fib' _ acc2 1 = acc2
    fib' acc1 acc2 n = fib' acc2 (acc1 + acc2) (n - 1)
