
data Dir = Left' | Right' | Straight'  deriving (Show)

data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a) deriving (Show)

mapa :: Mapa [Char]
mapa = Bifurcacion ["Pala", "Botella"]
                   (Nada (Bifurcacion ["Monedas", "Espejo"]
                                      (Bifurcacion [""]
                                                   (Nada (Cofre [""]))
                                                   (Bifurcacion [""]
                                                                (Cofre [""])
                                                                (Bifurcacion [""]
                                                                             (Cofre ["Caca"])
                                                                             (Cofre ["Caca"])
                                                                )
                                                   )
                                      )
                                      (Cofre ["Monos"])
                         )
                   )
                   (Bifurcacion []
                                (Cofre [])
                                (Cofre ["Bananas"]))

mapa2 = Bifurcacion [""]
                    (Nada (Cofre [""]))
                    (Cofre [""])

-- 1

--a
objects :: Mapa a -> [a]
objects (Cofre xs) = xs
objects (Nada m) = objects m
objects (Bifurcacion xs ml mr) = xs ++ (objects ml) ++ (objects mr)

--b
mapM' :: (a -> b) -> Mapa a -> Mapa b
mapM' f (Cofre xs) = Cofre (map f xs)
mapM' f (Nada m) = Nada (mapM' f m)
mapM' f (Bifurcacion xs ml mr) = Bifurcacion (map f xs) (mapM' f ml) (mapM' f mr)

--c
hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f (Cofre xs)             []             = any f xs
hasObjectAt f (Nada m)               []             = False
hasObjectAt f (Bifurcacion xs ml mr) []             = any f xs
hasObjectAt f (Nada m) (ds)                         = hasObjectAt f m ds
hasObjectAt f (Bifurcacion xs ml mr) (Left':ds)     = hasObjectAt f ml ds
hasObjectAt f (Bifurcacion xs ml mr) (Straight':ds) = any f xs
hasObjectAt f (Bifurcacion xs ml mr) (Right':ds)    = hasObjectAt f mr ds

--d
longestPath :: Mapa a -> [Dir]
longestPath (Cofre xs) = []
longestPath (Nada m) = [Straight'] ++ longestPath m
longestPath (Bifurcacion xs ml mr) = [longestDir ml mr] ++ longestPath (maxMap ml mr)

longestDir :: Mapa a -> Mapa a -> Dir
longestDir m1 m2 = if (maxHeight m1) < (maxHeight m2)
                   then Right'
                   else Left'

maxMap :: Mapa a -> Mapa a -> Mapa a
maxMap m1 m2 = if (maxHeight m1) < (maxHeight m2)
               then m2
               else m1

maxHeight :: Mapa a -> Int
maxHeight (Cofre _) = 1
maxHeight (Nada m) = 1 + maxHeight m
maxHeight (Bifurcacion xs ml mr) = 1 + (max (maxHeight ml) (maxHeight mr))

-- 2
foldM :: ([a] -> b -> b -> b) -> (b -> b) -> ([a] -> b) -> Mapa a -> b
foldM fb fn fc (Cofre os)             = fc os
foldM fb fn fc (Nada m)               = fn (foldM fb fn fc m)
foldM fb fn fc (Bifurcacion os ml mr) = fb os (foldM fb fn fc ml)
                                              (foldM fb fn fc mr)
-- a
objects' :: Mapa a -> [a]
objects' = foldM (\os ml mr -> ml ++ os ++ mr) id id

-- b
mapM'' :: (a -> b) -> Mapa a -> Mapa b
mapM'' f m = foldM (\os ml mr -> Bifurcacion (map f os) ml mr)
                  id
                  (\os -> Cofre (map f os))
                  m

hasObjectAt' :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt' f m ds = (foldM (\os rml rmr -> (\ds -> foldr (\(d:ds) -> (if d == Left'
                                                                       then rml
                                                                       else rmr))
                                                           (\_ -> any f os)
                                                           ds))
                             (\rm -> (\(d:ds) -> rm ds))
                             (\os ds -> any f os)
                             m) ds
