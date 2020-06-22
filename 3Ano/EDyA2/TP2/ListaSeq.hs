import Par ((|||))

import Seq
                   
instance Seq [] where 
    emptyS     = emptyL
    singletonS = singletonL
    lengthS    = lengthL 
    nthS       = nthL 
    tabulateS  = tabulateL
    mapS       = mapL 
    filterS    = filterL 
    appendS    = appendL
    takeS      = takeL
    dropS      = dropL
    showtS     = showtL
    showlS     = showlL
    joinS      = joinL
    reduceS    = reduceL
    scanS      = scanL
    fromList   = fromListL

emptyL :: [a]
emptyL = []

singletonL :: a -> [a]
singletonL a = [a]

lengthL :: [a] -> Int
lengthL []     = 0
lengthL (x:xs) = 1 + lengthL xs

nthL :: [a] -> Int -> a
nthL (x:xs) n | n==0 = x
              | otherwise = nthL xs (n-1)

tabulateL' :: (Int -> a) -> Int -> Int -> [a]
tabulateL' _ _ 0 = []
tabulateL' f i j = let (x,xs) = (f i) ||| (tabulateL' f (i+1) (j-1))
                   in (x:xs) 

tabulateL :: (Int -> a) -> Int -> [a]
tabulateL f n = tabulateL' f 0 n

mapL :: (a -> b) -> [a] -> [b]
mapL _ [] = []
mapL f (x:xs) = let (y,ys) = f x ||| mapL f xs
                in (y:ys)

filterL :: (a -> Bool) -> [a] -> [a]
filterL _ [] = []
filterL f (x:xs) = let (y,ys) =  (f x) ||| filterL f xs
                   in if y then x:ys else ys

appendL :: [a] -> [a] -> [a]  
appendL x [] = x 
appendL [] y = y
appendL (x:xs) y = x:(appendL xs y)

takeL :: [a] -> Int -> [a] 
takeL [] _ = []
takeL _ 0 = []
takeL (x:xs) n = x:(takeL xs (n-1))

dropL :: [a] -> Int -> [a] 
dropL [] _ = []
dropL x 0 = x
dropL (x:xs) n = dropL xs (n-1)

showtL :: [a] -> TreeView a ([a])
showtL [] = EMPTY
showtL [x] = ELT x
showtL l = let n = lengthL l
               m = div n 2
               (t1,t2) = takeL l m ||| dropL l m
               in NODE t1 t2

showlL :: [a] -> ListView a ([a])
showlL [] = NIL
showlL (x:xs) = CONS x xs

joinL :: [[a]] -> [a]
joinL [] = []
joinL (x:xs) = appendL x (joinL xs)

contractL :: (a -> a -> a) -> [a] -> [a]
contractL f []       = []
contractL f [x]      = [x]
contractL f (x:y:xs) = let (x',xs') = (f x y) ||| (contractL f xs)
                       in (x':xs')

reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL f e [] = e
reduceL f e [x] = f e x
reduceL f e s = let xs = contractL f s
                in reduceL f e xs           

expandL :: (a -> a -> a) -> [a] -> [a] -> [a]
expandL f [] _ = []
expandL f [x] (y:ys) = [y]
expandL f (x1:x2:xs) (y:ys) = let (x',xs') = f y x1 ||| expandL f xs ys  
                              in y:x':xs'

scanL :: (a -> a -> a) -> a -> [a] -> ([a],a)
scanL f e []  = ([],e)
scanL f e [x] = ([e],f e x)
scanL f e s = let xs = contractL f s
                  (s',t) = scanL f e xs
              in (expandL f s s', t)


fromListL :: [a] -> [a]
fromListL l = l





