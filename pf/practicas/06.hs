
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- 4) Fold TipTree
--
-- De el tipo y defina la función foldTT que generaliza la recursión sobre la
-- estructura TipTree (definidos en la práctica 4).
-- Luego defina las siguientes funciones sobre la estructura sin utilizar
-- recursión explícita (i.e., usando foldTT)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving (Show)

tree :: (TipTree Int)
tree = Join
            (Join
                  (Join
                        (Tip 2)
                        (Join
                              (Tip 3)
                              (Join
                                    (Tip 5)
                                    (Tip 4))))
                  (Tip 1))
            (Join
                  (Join
                        (Tip 2)
                        (Join
                              (Tip 3)
                              (Join
                                    (Tip 4)
                                    (Join
                                          (Tip 5)
                                          (Tip 5)))))
                  (Join
                        (Join
                              (Tip 3)
                              (Join
                                    (Tip 4)
                                    (Tip 4)))
                        (Tip 2)))

foldTT :: (b -> b -> b) -> (a -> b) -> TipTree a -> b
foldTT fj ft (Tip x) = ft x
foldTT fj ft (Join t1 t2) = fj (foldTT fj ft t1) (foldTT fj ft t2)

sizeTT :: TipTree a -> Int
sizeTT = foldTT (\sl sr -> 1 + sl + sr) (const 1)

walkthroughTT :: TipTree a -> [a]
walkthroughTT = foldTT (\xsl xsr -> xsl ++ xsr) (\x -> x:[])

mirrorTT :: TipTree a -> TipTree a
mirrorTT = foldTT (\t1 t2 -> (Join t2 t1)) Tip

mapTT :: (a -> b) -> TipTree a -> TipTree b
mapTT f = foldTT (\tl tr -> Join tl tr) (\x -> Tip (f x))


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- 3) Fold Logical
-- De el tipo y defina la función foldLogic que generaliza la recursión sobre la
-- estructura Logical (definida en la práctica 4). Luego redefina las funciones
-- sobre la estructura sin utilizar recursión explícita (i.e., usando foldLogic)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type Variable = Int

data Logical = Var Variable
             | Not Logical
             | And Logical Logical
             | Or Logical Logical
             deriving (Show)

type Valuation = Variable -> Bool

-- A valuation sample
valuation :: Variable -> Bool
valuation 1 = True
valuation 2 = True
valuation _ = False

-- Logical values samples
expression0 :: Logical
expression0 = And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 4))

expression1 :: Logical
expression1 = And (Or (Var 4) (Not (Not (Var 4)))) (Or (Not (Not (Var 4))) (Var 4))

-- foldLogic ::

foldLogic :: (Variable -> a) -> (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Logical -> a
foldLogic fv fn fa fo (Var x) = fv x
foldLogic fv fn fa fo (Not l) = fn (foldLogic fv fn fa fo l)
foldLogic fv fn fa fo (And l1 l2) = fa (foldLogic fv fn fa fo l1) (foldLogic fv fn fa fo l2)
foldLogic fv fn fa fo (Or l1 l2) = fo (foldLogic fv fn fa fo l1) (foldLogic fv fn fa fo l2)


lEval :: Valuation -> Logical -> Bool
lEval v = foldLogic v not (&&) (||)

vars :: Logical -> [Int]
vars = foldLogic (\x -> [x]) id (++) (++)
