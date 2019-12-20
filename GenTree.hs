data AG a = GNode a [AG a] deriving Show

foldAG0 :: (a->[b]->b) -> AG a -> b
foldAG0 h (GNode x ts) = h x (map (foldAG0 h) ts)

foldAG1 :: (a->c->b) -> (b->c->c) -> c -> AG a -> b
foldAG1 g f z (GNode x ts) = g x (foldr f z (map (foldAG1 g f z) ts))

foldAG2 :: (a->c->b) -> ([b]->c) -> AG a -> b
foldAG2 g k (GNode x ts) = g x (k (map (foldAG2 g k) ts))

ejemplo :: AG Int
ejemplo = GNode 2 [child0, child1, child2]
child0 :: AG Int
child0 = GNode 4 [grandkid0, grandkid1, grandkid2,grandkid3]
child1 :: AG Int
child1 = GNode 8 [grandkid4, grandkid5, grandkid6]
child2 :: AG Int
child2 = GNode 12 [grandkid7]
grandkid0 :: AG Int
grandkid0 = GNode 16 [GNode 10 []]
grandkid1 :: AG Int
grandkid1 = GNode 17 [GNode 100 [], GNode 101 []]
grandkid2 :: AG Int
grandkid2 = GNode 19 [GNode 102 [GNode 13 []], GNode 101 [GNode 14 []]]
grandkid3 :: AG Int
grandkid3 = GNode 23 []
grandkid4 :: AG Int
grandkid4 = GNode 42 []
grandkid5 :: AG Int
grandkid5 = GNode 0 [] 
grandkid6 :: AG Int
grandkid6 = GNode 1 []
grandkid7 :: AG Int
grandkid7 = GNode 3 []

--GNode 18 [GNode 19 [GNode 20 [GNode 32 [GNode 64 []]]]]

mapGT :: (a -> b) -> AG a -> AG b
mapGT f = foldAG0 (GNode . f)

mapGT' :: (a -> b) -> AG a -> AG b
mapGT' f (GNode x xs) = GNode (f x) (map (mapGT' f) xs)   

sumGT :: AG Int -> Int
sumGT = foldAG0 (\n ns -> n + sum ns)

sumGT' :: AG Int -> Int
sumGT' (GNode x xs) = x + sum (map sumGT' xs) 

--sizeGT :: AG a -> Int
sizeGT' :: AG a -> Int
sizeGT' (GNode x xs) = 1 + length (map sizeGT' xs) 

--heightGT :: GenTree a -> Int
heightGT' :: AG a -> Int
heightGT' (GNode x xs) = case xs of 
							[] -> 0
							(x:xs) -> maximum (1 : map heightGT' xs)

--preOrderGT :: GenTree a -> [a]
--inOrderGT :: GenTree a -> [a]
--postOrderGT :: GenTree a -> [a]

mirrorGT :: AG a -> AG a
mirrorGT = foldAG0 (\x cs -> GNode x (reverse cs))

--countByGT :: (a -> Bool) -> GenTree a -> Int
--partitionGT:: (a -> Bool) -> GenTree a -> ([a], [a])
--zipWithGT:: (a->b->c) -> GenTree a -> GenTree b -> GenTree c
--caminoMasLargoGT :: GenTree a -> [a]
--todosLosCaminosGT :: GenTree a -> [[a]]
--todosLosNivelesGT :: GenTree a -> [[a]]
--caminoHastaGT :: Eq a => a -> GenTree a -> [a]
--nivelNGT :: GenTree a -> Int -> [a]

