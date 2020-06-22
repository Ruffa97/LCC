module Tp1 where

-- class Dic k v d | d -> k v where
--     vacio :: d
--     insertar :: Ord k => k -> v -> d -> d
--     buscar :: Ord k => k -> d -> Maybe v
--     eliminar :: Ord k => k -> d -> d
--     claves :: Ord k => d -> [[k]]




t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                           (Node 'o' (Just 2) (Leaf 'd' 9)
                                                               E
                                                              (Leaf 's' 4))
                                           E)
                       (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                              (Leaf 'n' 7)
                                                                E)
                                           E)

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v)
                 | Leaf k v
                 | E
                 deriving Show


-- devuelve el valor asociado a una clave
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [] _ = Nothing
search [k] (Leaf k' v) | k == k' = Just v
search (k:ks) (Leaf _ _) = Nothing --Lego a una hoja pero aun tengo resto de una clave
search [k]    (Node k' v tti ttm ttd) | k == k'   = v
search (k:ks) (Node k' v tti ttm ttd) | k == k'   = search ks ttm
                                      | k > k'    = search (k:ks) ttd
                                      | otherwise = search (k:ks) tti


-- agrega un par (clave, valor) a un árbol. Si la clave ya
-- está en el árbol, actualiza su valor
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [k]    v E = Leaf k v
insert (k:ks) v E = Node k Nothing E (insert ks v E) E
insert [k]    v (Leaf k' v') | k == k' = Leaf k v
                             | k > k' =  Node k' (Just v') E E (insert [k] v E)
                             | otherwise = Node k' (Just v') (insert [k] v E) E E
insert (k:ks) v (Leaf k' v') | k == k'   = Node k' (Just v') E (insert ks v E) E
                             | k > k'    = Node k' (Just v') E E (insert [k] v E)
                             | otherwise = Node k' (Just v') (insert [k] v E) E E
insert [k]    v (Node k' v' tti ttm ttd) | k == k'   = Node k' (Just v) tti ttm ttd
                                         | k > k'    = Node k' v' tti ttm (insert [k] v ttd)
                                         | otherwise = Node k' v' (insert [k] v tti) ttm  ttd
insert (k:ks) v (Node k' v' tti ttm ttd) | k == k'   = Node k' v' tti (insert ks v ttm) ttd
                                         | k > k'    = Node k' v' tti ttm (insert (k:ks) v  ttd)
                                         | otherwise = Node k' v' (insert (k:ks) v  tti) ttm ttd



-- elimina una clave y el valor asociada a ésta en un árbol.
delete :: Ord k => [k ] -> TTree k v -> TTree k v
delete _ E    = E
delete [k]    (Leaf k' v) | k == k' = E
delete (k:ks) t@(Leaf a b) = t
delete [k]    (Node k' v tti ttm ttd) | k == k'   = Node k' Nothing tti ttm ttd
delete (k:ks) (Node k' v tti ttm ttd) | k == k'   = Node k' v tti (delete ks ttm) ttd
                                      | k > k'    = Node k' v tti ttm (delete (k:ks) ttd)
                                      | otherwise = Node k' v (delete (k:ks) tti) ttm ttd

keys' ::Eq v => [[k]] -> [k] -> TTree k v -> [[k]]
keys' kys ky E = []
keys' kys ky (Leaf k v) = kys ++ [ky++[k]]
keys' kys ky (Node k v tti ttm ttd) = (keys' kys ky tti) ++ addKey ++ (keys' kys ky ttd)
                 where addKey = if v == Nothing
                                  then (keys' kys (ky ++ [k]) ttm)
                                  else kys ++ [ky ++ [k]] ++ (keys' kys (ky ++ [k]) ttm)

-- dado un árbol devuelve una lista ordenada con las claves del mismo
keys ::(Eq v) => TTree k v -> [[k]]
keys tt = keys' [] [] tt
