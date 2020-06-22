import Seq
import Par
import qualified Arr as A
import Arr ((!))

instance Seq A.Arr where
        emptyS     = emptyA
        singletonS = singletonA
        lengthS    = lengthA
        nthS       = nthA
        tabulateS  = tabulateA
        mapS       = mapA
        filterS    = filterA
        appendS    = appendA
        takeS      = takeA
        dropS      = dropA
        showtS     = showtA
        showlS     = showlA
        joinS      = joinA
        reduceS    = reduceA
        scanS      = scanA
        fromList   = fromListA

emptyA :: A.Arr a
emptyA = A.empty

singletonA :: a -> A.Arr a
singletonA a = A.fromList [a]

lengthA :: A.Arr a -> Int 
lengthA a = A.length a

nthA :: A.Arr a -> Int -> a
nthA a x = a!x 

tabulateA :: (Int -> a) -> Int -> A.Arr a
tabulateA f x = A.tabulate f x

mapA :: (a -> b) -> A.Arr a -> A.Arr b
mapA f a = let l = lengthA a
            in tabulateA (\i -> f(a!i)) l             
    
filterA :: (a -> Bool) -> A.Arr a -> A.Arr a
filterA f a = let l = lengthA a 
                  s = tabulateA (\i -> if (f(a!i)) then singletonA (a!i) else emptyA) l
            in A.flatten s

appendA :: A.Arr a -> A.Arr a -> A.Arr a
appendA a b = let l1 = lengthA a
                  l2 = lengthA b
                  l  = l1 + l2
               in tabulateA (\i -> if i<l1 then a!i else b!(i-l1)) l

takeA :: A.Arr a -> Int -> A.Arr a
takeA a n = A.subArray 0 n a 

dropA :: A.Arr a -> Int -> A.Arr a
dropA a n = A.subArray n ((lengthA a) - n) a

showtA :: A.Arr a -> TreeView a (A.Arr a)
showtA a = let l = lengthA a 
            in case l of 
                0 -> EMPTY
                1 -> ELT (a!0)
                otherwise -> let m = div l 2
                                 (t1, t2) = takeA a m ||| dropA a m
                                 in NODE t1 t2

showlA :: A.Arr a -> ListView a (A.Arr a)
showlA a = let l = lengthA a
            in case l of
                0 -> NIL
                otherwise -> CONS (a!0) (dropA a 1) 

joinA :: A.Arr (A.Arr a) -> A.Arr a
joinA a = A.flatten a

rdcAux :: (a -> a -> a)-> A.Arr a -> Int -> Int -> a
rdcAux f a i l = if i < div l 2
                  then (f (a!(2*i)) (a!(2*i+1)))
                  else (a!(2*i))

reduceA :: (a -> a -> a) -> a -> A.Arr a -> a
reduceA f e a = let l = A.length a
                   in case l of
                       0 -> e
                       1 -> f e (a!0)
                       otherwise -> let xs = tabulateA (\i -> rdcAux f a i l) ((div l 2)+(mod l 2))
                                      in reduceA f e xs

expandA :: (a -> a -> a) -> A.Arr a -> A.Arr a -> A.Arr a
expandA f a a' = let l1 = lengthA a
                    in tabulateA (\i -> if (even i)
                                        then (a'!(div i 2))
                                        else (f (a'!(div i 2)) (a!(i-1)))) l1

scanA :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a,a)
scanA f e a = let l = lengthA a
                in case l of
                        0 -> (emptyA, e)
                        1 -> (singletonA e, f e (a!0))
                        otherwise -> let xs = tabulateA (\i -> rdcAux f a i l) ((div l 2)+(mod l 2))
                                         (s,t) = scanA f e xs
                                         in (expandA f a s, t)

fromListA :: [a] -> A.Arr a
fromListA a = A.fromList a
