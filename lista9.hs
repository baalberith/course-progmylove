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
maxSeqLen = maxSeqLen' limit 0 where 
    limit = 10 ^ 4
    seqLen prev len =
        if prev == 1 then 
            len
        else
            if even prev 
                then seqLen (prev `div` 2) (len + 1)
                else seqLen (3 * prev + 1) (len + 1)
    maxSeqLen' 0 maxLen = maxLen
    maxSeqLen' n maxLen = 
        if len > maxLen
            then maxSeqLen' (n - 1) len
            else maxSeqLen' (n - 1) maxLen
        where len = seqLen n 1
                    
maxSeqLen' :: Integer
maxSeqLen' = maxSeqLen'' limit 0 0 0 limit where 
    limit = 10 ^ 4
    maxSeqLen'' _    _   _    maxlen 0 = maxlen
    maxSeqLen'' prev len maxn maxlen n =
        if prev == maxn then 
            maxSeqLen'' (n - 1) 1 n (maxlen + len - 1) (n - 1)
        else
            if prev == 1 then
                if len > maxlen 
                    then maxSeqLen'' (n - 1) 1 n len (n - 1)
                    else maxSeqLen'' (n - 1) 1 maxn maxlen (n - 1)           
            else
                if even prev 
                    then maxSeqLen'' (prev `div` 2) (len + 1) maxn maxlen n
                    else maxSeqLen'' (3 * prev + 1) (len + 1) maxn maxlen n
  

-- zadanie 4

digSum :: Integer
digSum = digSum' 0 (fact number) where 
    number = 100
    fact 0 = 1
    fact n = n * fact (n - 1)
    digSum' acc n = 
        if n == 0 
            then acc
            else digSum' (acc + (n `mod` 10)) (n `div` 10)
    
 
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
