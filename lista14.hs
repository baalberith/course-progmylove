-- zadanie 1 

data Cyclist a = Elem (Cyclist a) a (Cyclist a) deriving Show

                          
fromList :: [a] -> Cyclist a
fromList [] = undefined
fromList xs = 
    let (first, last) = makeList last xs first 
    in first 

makeList :: Cyclist a -> [a] -> Cyclist a -> (Cyclist a, Cyclist a)
makeList prev ([x]) next = 
    let this = Elem prev x next 
    in (this, this) 
makeList prev (x:xs) next = 
    let this = Elem prev x rest 
        (rest, last) = makeList this xs next 
    in (this, last) 
    

label :: Cyclist a -> a
label (Elem _ label _ ) = label 

forward :: Cyclist a -> Cyclist a
forward (Elem _ _ next) = next 

backward :: Cyclist a -> Cyclist a
backward (Elem prev _ _) = prev 


ex1 :: Integer
ex1 = label . forward . forward . forward . backward . forward . forward $ fromList [1,2,3]


-- zadanie 2 

enumInts :: Cyclist Integer
enumInts = Elem neg 0 pos where 
    neg = negative enumInts (-1) 
    pos = positive enumInts 1 

negative :: (Num a) => Cyclist a -> a -> Cyclist a
negative gt x = this where 
    this = Elem lt x gt
    lt = negative this (x-1) 

positive :: (Num a) => Cyclist a -> a -> Cyclist a
positive lt x = this where 
    this = Elem lt x gt 
    gt = positive this (x+1) 


ex2 :: Integer
ex2 = label . forward . forward $ enumInts
    
    
-- zadanie 3 

newtype Cyc a b = Cyc (Cyclist a -> (b, Cyclist a)) 

instance Monad (Cyc a) where 
    (Cyc comp) >>= f = 
        Cyc (\st -> 
            let (x, st') = comp st 
                Cyc comp' = f x                                  
            in comp' st') 
    return x = Cyc (\st -> (x, st)) 


runCyc :: Cyclist a -> (Cyc a b) -> b 
runCyc st (Cyc comp) = fst $ comp st 

fwd :: Cyc a () 
fwd = Cyc (\st -> ((), forward st)) 

bkw :: Cyc a () 
bkw = Cyc (\st -> ((), backward st))

lbl :: Cyc a a
lbl = Cyc (\st -> (label st, st)) 


ex3 :: Integer
ex3 = runCyc enumInts $ do 
    bkw 
    bkw 
    bkw 
    bkw 
    x <- lbl 
    fwd 
    fwd 
    y <- lbl 
    fwd 
    z <- lbl 
    return $ x + y + z 


-- zadanie 4

newtype Random a = Random (Int -> (a, Int))

instance Monad Random where 
    (Random comp) >>= f = 
        Random (\seed -> 
            let (val, seed') = comp seed 
                Random comp' = f val 
            in comp' seed') 
    return val = Random (\seed -> (val, seed)) 


init' :: Int -> Random () 
init' seed = Random (\_ -> ((), seed)) 

random' :: Random Int 
random' = 
    Random (\seed -> 
        let newseed = 16807 * (seed `mod` 127773) - 2836 * (seed `div` 127773) 
            val = if newseed > 0 then newseed else (newseed + 2147483647) 
        in (val, val)) 


runRandom :: Int -> Random a -> a
runRandom seed (Random comp) = fst $ comp seed
 
ex4 :: Int
ex4 = runRandom 0 $ do 
    init' 0
    r <- random' 
    return r


-- zadanie 5

newtype SSC a = SSC (String -> (a,String)) 

instance Monad SSC where 
    (SSC comp) >>= f =    
        SSC (\str -> 
            let (val, str') = comp str 
                SSC comp' = f val 
            in comp' str') 
    return val = SSC (\str -> (val, str)) 


runSSC :: SSC a -> String -> a 
runSSC (SSC comp) str = fst $ comp str 

getc :: SSC Char 
getc = SSC (\str -> (head str, tail str)) 

ungetc :: Char -> SSC () 
ungetc ch = SSC (\str -> ((), ch:str)) 

isEOS :: SSC Bool 
isEOS = SSC (\str -> let val = null str in (val, str)) 


countLines :: String -> Int 
countLines = runSSC $ lines' 0

lines' :: Int -> SSC Int 
lines' n = do 
    eos <- isEOS 
    if eos 
        then return n 
        else do 
            ch <- getc 
            lines' (if ch == '\n' then n + 1 else n) 


ex5 = countLines "ala ma kota,\n a ola ma psa\n" 


-- zadanie 6

newtype StateComput s a = SC (s -> (a,s)) 

instance Monad (StateComput s) where 
    (SC comp) >>= f = 
        SC (\st -> 
            let (val, st') = comp st 
                SC comp' = f val 
            in comp' st') 
    return val = SC (\st -> (val, st)) 
    
    