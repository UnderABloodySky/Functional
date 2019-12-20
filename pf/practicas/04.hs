
-- ************************************
-- 2) Árboles
-- ************************************

data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving (Show)

tree :: (TipTree Int)
tree = Join
            (Join
                  (Join
                        (Tip 2)
                        (Join
                              (Tip 3)
                              (Join
                                    (Tip 4)
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

tree2 = Join
            (Join
                  (Join
                        (Tip 2)
                        (Join
                              (Join
                                    (Tip 4)
                                    (Tip 4))
                              (Tip 3)))
                  (Join
                        (Join
                              (Join
                                    (Join
                                          (Tip 5)
                                          (Tip 5))
                                    (Tip 4))
                              (Tip 3))
                        (Tip 2)))
            (Join
                  (Tip 1)
                  (Join
                        (Join
                              (Join
                                    (Tip 4)
                                    (Tip 4))
                              (Tip 3))
                        (Tip 2)))

tree3 = Join
            (Join
                  (Tip 3)
                  (Tip 4))
            (Join
                  (Join
                        (Tip 6)
                        (Tip 7))
                  (Tip 5))

heightTip :: (TipTree a) -> Int
heightTip (Tip _)                = 0
heightTip (Join (Tip _) (Tip _)) = 1
heightTip (Join t1 t2)           = 1 + (max (heightTip t1) (heightTip t2))

leaves :: (TipTree a) -> Int
leaves (Tip _)      = 1
leaves (Join t1 t2) = (leaves t1) + leaves t2

nodes :: (TipTree a) -> Int
nodes (Tip _) = 0
nodes (Join t1 t2) = 1 + (nodes t1) + nodes t2

walkover :: (TipTree a) -> [TipTree a]
walkover (Tip n) = [Tip n]
walkover (Join t1 t2) = (walkover t1) ++ (walkover t2)

mirrorTip :: (TipTree a) -> TipTree a
mirrorTip (Tip l)          = Tip l
mirrorTip (Join t1 t2)     = Join (mirrorTip t2) (mirrorTip t1)

mapTip :: (a -> b) -> (TipTree a) -> TipTree b
mapTip f (Tip l) = Tip (f l)
mapTip f (Join t1 t2) = Join (mapTip f t1) (mapTip f t2)

-- ************************************
-- 3) Polinomios
-- ************************************

data Poli = Cte Int | PoliVar | Add Poli Poli | Mul Poli Poli deriving (Show)

-- (Add (Mul Var Var) (Add (Mul (Cte 3) Var) (Cte 5)))

-- a) Retorna el resultado de evaluar un polinomio P con un valor x dado (P (x))
eval :: Poli -> Int -> Int
eval (Cte c) _ = c
eval PoliVar x = x
eval (Add p1 p2) x = (+) (eval p1 x) (eval p2 x)
eval (Mul p1 p2) x = (*) (eval p1 x) (eval p2 x)

-- b) Retorna el polinomio obtenido luego de multiplicar cada constante y
--    variable por un valor entero.
mEscalar :: Poli -> Int -> Poli
mEscalar (Cte c) x     = Mul (Cte c) (Cte x)
mEscalar PoliVar x     = Mul PoliVar (Cte x)
mEscalar (Add p1 p2) x = Add (mEscalar p1 x) (mEscalar p2 x)
mEscalar (Mul p1 p2) x = Mul (mEscalar p1 x) (mEscalar p2 x)

--c) Retorna un polinomio equivalente al tomado como parámetro pero donde las
--   operaciones de suma y multiplicación entre constantes ya fueron resueltas,
--   es decir, un polinomio en donde no existen constructores de la forma:
--   Add (Cte _) (Cte _) ni Mul (Cte _) (Cte _)
sOptimize :: Poli -> Poli
sOptimize (Cte c) = Cte c
sOptimize PoliVar = PoliVar
sOptimize (Add (Cte x) (Cte y)) = Cte ((+) x y)
sOptimize (Mul (Cte x) (Cte y)) = Cte ((*) x y)
sOptimize (Add p1 p2) = Add (sOptimize p1) (sOptimize p2)
sOptimize (Mul p1 p2) = Mul (sOptimize p1) (sOptimize p2)

-- mEscalar reduction
-- eval (mEscalar poli 3) 2
-- eval (mEscalar (Add (Mul PoliVar PoliVar) (Add (Mul (Cte 3) PoliVar) (Cte 5))) 3) 2
-- eval (Add (mEscalar (Mul PoliVar PoliVar) 3) (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3)) 2
-- (+) (eval (mEscalar (Mul PoliVar PoliVar) 3) 2) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) (eval (Mul (mEscalar PoliVar 3) (mEscalar PoliVar 3)) 2) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) (eval (mEscalar PoliVar 3) 2) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) (eval (Mul PoliVar (Cte 3)) 2) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) ((*) (eval PoliVar 2) (eval (Cte 3) 2)) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) ((*) 2 (eval (Cte 3) 2)) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) ((*) 2 3) (eval (Mul PoliVar (Cte 3)) 2)) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) ((*) 2 3) ((*) (eval PoliVar 2) (eval (Cte 3) 2))) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) ((*) 2 3) ((*) 2 (eval (Cte 3) 2))) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) ((*) 2 3) ((*) 2 3)) (eval (mEscalar (Add (Mul (Cte 3) PoliVar) (Cte 5)) 3) 2)
-- (+) ((*) ((*) 2 3) ((*) 2 3)) (eval (Add (mEscalar (Mul (Cte 3) PoliVar) 3) (mEscalar (Cte 5) 3)) 2)
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) (eval (mEscalar (Mul (Cte 3) PoliVar) 3) 2) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) (eval (Mul (mEscalar (Cte 3) 3) (mEscalar PoliVar 3)) 2) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) (eval (mEscalar (Cte 3) 3) 2) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) (eval (Mul (Cte 3) (Cte 3)) 2) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) (eval (Cte 3) 2) (eval (Cte 3) 2)) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 (eval (Cte 3) 2)) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) (eval (mEscalar PoliVar 3) 2)) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) (eval (Mul PoliVar (Cte 3)) 2)) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) ((*) (eval PoliVar 2) (eval (Cte 3) 2))) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) ((*) 2 3)) (eval (mEscalar (Cte 5) 3) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) ((*) 2 3)) (eval (Mul (Cte 5) (Cte 3)) 2))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) ((*) 2 3)) ((*) (eval (Cte 5) 2) (eval (Cte 3) 2)))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) ((*) 2 3)) ((*) 5 (eval (Cte 3) 2)))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) ((*) 2 3)) ((*) 5 3))
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) ((*) 2 3)) 15)
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) ((*) 3 3) 6) 15)
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) ((*) 9 6) 15)
-- (+) ((*) ((*) 2 3) ((*) 2 3)) ((+) 54 15)
-- (+) ((*) ((*) 2 3) ((*) 2 3)) 69
-- (+) ((*) ((*) 2 3) 6) 69
-- (+) ((*) 6 6) 69
-- (+) 36 69
-- 105

poli = Add (Mul PoliVar PoliVar) (Add (Mul (Cte 3) PoliVar) (Cte 5))
poli2 = Add (Mul (Cte 3) (Cte 3)) (Add (Cte 6) (Cte 5))
poli3 = Add (Mul (Cte 3) (Cte 3)) (Add (Mul (Cte 3) PoliVar) (Cte 5))

-- ************************************
-- 3) Fórmulas lógicas
-- ************************************

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

-- a) Retorna el resultado de resolver la expresión lógica con la valuación dada.
lEval :: Logical -> Valuation -> Bool
lEval (Var x)     v = v x
lEval (Not l)     v = not (lEval l v)
lEval (And l1 l2) v = (lEval l1 v) && (lEval l2 v)
lEval (Or l1 l2)  v = (lEval l1 v) || (lEval l2 v)

-- Reduction: lEval
-- lEval expression0 valuation
-- lEval (And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 4))) valuation
-- (lEval (Or (Var 1) (Var 2)) valuation) && (lEval (Or (Not (Var 3)) (Var 4)) valuation)
-- (lEval (Var 1) valuation || lEval (Var 1) valuation) && (lEval (Or (Not (Var 3)) (Var 4)) valuation)
-- (valuation 1 || lEval (Var 1) valuation) && (lEval (Or (Not (Var 3)) (Var 4)) valuation)
-- (valuation 1 || valuation 1) && (lEval (Or (Not (Var 3)) (Var 4)) valuation)
-- (True || valuation 1) && (lEval (Or (Not (Var 3)) (Var 4)) valuation)
-- (True || True) && (lEval (Or (Not (Var 3)) (Var 4)) valuation)
-- True && (lEval (Or (Not (Var 3)) (Var 4)) valuation)
-- True && (lEval (Not (Var 3)) valuation || lEval (Var 4) valuation)
-- True && (not (lEval (Var 3) valuation) || lEval (Var 4) valuation)
-- True && (not (valuation 3) || lEval (Var 4) valuation)
-- True && (not False || lEval (Var 4) valuation)
-- True && (True || lEval (Var 4) valuation)
-- True && (True || valuation 4)
-- True && (True || False)
-- True && True
-- True

-- b) Retorna la lista de todas las variables con ocurrencias en una expresión
--    dada.
vars :: Logical -> [Int]
vars (Var x)     = [x]
vars (Not l)     = vars l
vars (And l1 l2) = vars l1 ++ vars l2
vars (Or l1 l2)  = vars l1 ++ vars l2

-- Reduction: vars
-- vars expression0
-- vars (And (Or (Var 1) (Var 2)) (Or (Not (Var 3)) (Var 4)))
-- vars (Or (Var 1) (Var 2)) ++ vars (Or (Not (Var 3)) (Var 4))
-- vars (Var 1) ++ vars (Var 2) ++ vars (Or (Not (Var 3)) (Var 4))
-- [1] ++ vars (Var 2) ++ vars (Or (Not (Var 3)) (Var 4))
-- [1] ++ [2] ++ vars (Or (Not (Var 3)) (Var 4))
-- [1] ++ [2] ++ vars (Not (Var 3)) ++ vars (Var 4)
-- [1] ++ [2] ++ vars (Var 3) ++ vars (Var 4)
-- [1] ++ [2] ++ [3] ++ vars (Var 4)
-- [1] ++ [2] ++ [3] ++ [4]
-- [1] ++ [2] ++ [3, 4]
-- [1] ++ [2, 3, 4]
-- [1, 2, 3, 4]

-- c) Simplifica las expresiones eliminando operaciones triviales, más
--    específicamente la doble negación y la conjunción o disyunción de
--    variables iguales.
simp :: Logical -> Logical
simp (Var x)               = Var x
simp (Not (Not l))         = l
simp (Not l)               = Not l
simp (And (Var x) (Var y)) = if x == y then Var x else And (Var x) (Var y)
simp (And l1 l2)           = simp (And (simp l1) (simp l2))
simp (Or (Var x) (Var y))  = if x == y then Var x else Or (Var x) (Var y)
simp (Or l1 l2)            = simp (Or (simp l1) (simp l2))

-- simp expression1
-- simp (And (Or (Var 4) (Not (Not (Var 4)))) (Or (Not (Not (Var 4))) (Var 4)))
-- simp (simp (And (simp (Or (Var 4) (Not (Not (Var 4))))) (simp (Or (Not (Not (Var 4))) (Var 4)))))
-- simp (simp (And (simp (Or (simp (Var 4)) (simp (Not (Not (Var 4)))))) (simp (Or (Not (Not (Var 4))) (Var 4)))))
-- simp (simp (And (simp (Or (simp (Var 4)) (simp (Not (Not (Var 4)))))) (simp (Or (Not (Not (Var 4))) (Var 4)))))
-- simp (simp (simp (And (simp (simp (Or (simp (Var 4)) (simp (Not (Not (Var 4))))))) (simp (simp (Or (Not (Not (Var 4))) (Var 4)))))))
-- simp (simp (simp (And (simp (simp (Or (Var 4) (simp (Not (Not (Var 4))))))) (simp (simp (Or (Not (Not (Var 4))) (Var 4)))))))
-- simp (simp (simp (And (simp (simp (Or (Var 4) (simp (Var 4))))) (simp (simp (Or (Not (Not (Var 4))) (Var 4)))))))
-- simp (simp (simp (And (simp (simp (Or (Var 4) (Var 4)))) (simp (simp (Or (Not (Not (Var 4))) (Var 4)))))))
-- simp (simp (simp (And (Var 4) (simp (simp (Or (Not (Not (Var 4))) (Var 4)))))))
-- simp (simp (simp (And (Var 4) (simp (simp (Or (Var 4) (simp (Var 4))))))))
-- simp (simp (simp (And (Var 4) (simp (simp (Or (Var 4) (Var 4)))))))
-- simp (simp (simp (And (Var 4) (simp (Var 4)))))
-- simp (simp (simp (And (Var 4) (Var 4))))
-- simp (simp (Var 4))
-- simp (Var 4)
-- Var 4

-- ************************************
-- 5) Secuencias
-- ************************************

data Seq a = SNil
          | Unit a
          | Cat (Seq a) (Seq a)
          deriving (Show)

seq0 :: Num a => Seq a
seq0 = Cat (Unit 1) (Cat (Unit 2) (Cat (Cat (Unit 4) (Unit 4)) (Unit 3)))

seq1 :: Num a => Seq a
seq1 = Cat (Unit 1) (Unit 2)

seq2 :: Num a => Seq a
seq2 = Cat (Cat SNil SNil) (Cat SNil (Unit 3))

seq3 :: Num a => Seq a
seq3 = Cat (Cat SNil SNil) (Cat SNil (Cat (Cat SNil (Cat SNil SNil)) (Unit 100)))

-- a) Toma dos secuencias y devuelve su concatenación.
appSeq :: Seq a -> Seq a -> Seq a
appSeq s1 s2 = Cat s1 s2

-- b) Toma un elemento y una secuencia y devuelve la secuencia que tiene al
--    elemento dado como cabeza y a la secuencia dada como cola.
conSeq :: a -> Seq a -> Seq a
conSeq x s = Cat (Unit x) s

-- c) Calcula la cantidad de elementos de una secuencia.
lenSeq :: Seq a -> Int
lenSeq SNil = 0
lenSeq (Unit x) = 1
lenSeq (Cat s1 s2) = (+) (lenSeq s1) (lenSeq s2)

-- d) Toma una secuencia e invierte sus elementos.
revSeq :: Seq a -> Seq a
revSeq (Cat s1 s2) = Cat (revSeq s2) (revSeq s1)
revSeq s = s

-- e) Toma una secuencia y devuelve su primer elemento (es decir el de más a
--    la izquierda).
-- headSeq :: Seq a -> a
-- headSeq (Unit x) = x
-- headSeq (Cat s1 s2) = headSeqs s1 s2
--
-- headSeqs :: Seq a -> Seq a -> a
-- headSeqs (Unit x) _ = x
-- headSeqs SNil (Unit x) = x
-- headSeqs SNil s = headSeq s
-- headSeqs s SNil = headSeq s
-- headSeqs

-- headSeq :: Seq a -> a
-- headSeq (Unit x) = x
-- headSeq (Cat SNil SNil) =
-- headSeq (Cat (Unit x) _) = x
-- headSeq (Cat SNil (Unit x)) = x
-- headSeq (Cat SNil s) = headSeq s
-- headSeq (Cat s1 s2) = headSeq (Cat )

-- f) Remueve la cabeza de una secuencia.
tailSeq :: Seq a -> Seq a
tailSeq SNil = SNil
tailSeq (Unit x) = SNil
tailSeq (Cat s1 s2) = s2

-- g) Elimina todos los SNils innecesarios de una secuencia.
--    Por ejemplo, normSeq (Cat (Cat SNil (Unit 1)) SNil) = Unit 1
normSeq :: Seq a -> Seq a
normSeq (Unit x) = Unit x
normSeq (Cat (Cat SNil SNil) s) = normSeq s
normSeq (Cat s (Cat SNil SNil)) = normSeq s
normSeq (Cat SNil s) = normSeq s
normSeq (Cat s SNil) = normSeq s
normSeq s = normSeq s

-- h) Toma dos secuencias y devuelve True si ambas contienen los mismos valores,
--    en el mismo orden y en la misma cantidad.
-- eqSeq :: Seq a -> Seq a -> Bool

-- i) Toma una secuencia y devuelve una lista con los mismos elementos, en el
--    mismo orden.
-- seq2List :: Seq a -> [a]

-- j) ¿Qué ventajas y desventajas encuentra sobre (Seq a) respecto a las listas
--    de Haskell ([a])?

-- ************************************
-- 6) Árboles Generales
-- ************************************

data GenTree a = GNode a [GenTree a] deriving (Show)

-- a) Retorna la cantidad de elementos en el árbol.

-- Using Mutual Recursion
sizeG :: GenTree a -> Int
sizeG (GNode x ts) = 1 + sizeGs ts

sizeGs :: [GenTree a] -> Int
sizeGs [] = 0
sizeGs (t:ts) = sizeG t + sizeGs ts

-- de un saque >:D
sizeGT :: GenTree a -> Int
sizeGT (GNode x ts) = 1 + sum (map sizeGT ts)

-- b) Retorna la altura del árbol
heightGT :: GenTree a -> Int
heightGT (GNode x ts) = 1 + maxHeightGTS ts

maxHeightGTS :: [GenTree a] -> Int
maxHeightGTS [] = 0
maxHeightGTS (t:ts) = max (heightGT t) (maxHeightGTS ts)

-- Reduction: heightGT
-- heightGT (GNode 1 [(GNode 2 []), (GNode 2 [GNode 3 []]), (GNode 2 [])])
-- 1 + maxHeightGTS [(GNode 2 []), (GNode 2 [GNode 3 []]), (GNode 2 [])]
-- 1 + max (heightGT (GNode 2 [])) (maxHeightGTS [(GNode 2 [GNode 3 []]), (GNode 2 [])])
-- 1 + max (1 + maxHeightGTS []) (maxHeightGTS [(GNode 2 [GNode 3 []]), (GNode 2 [])])
-- 1 + max (1 + 0) (maxHeightGTS [(GNode 2 [GNode 3 []]), (GNode 2 [])])
-- 1 + max (1 + 0) (max (heightGT (GNode 2 [GNode 3 []])) (maxHeightGTS [GNode 2 []]))
-- 1 + max (1 + 0) (max (1 + maxHeightGTS [GNode 3 []]) (maxHeightGTS [GNode 2 []]))
-- 1 + max (1 + 0) (max (1 + (max (heightGT (GNode 3 [])) (maxHeightGTS []))) (maxHeightGTS [GNode 2 []]))
-- 1 + max (1 + 0) (max (1 + (max (1 + maxHeightGTS []) (maxHeightGTS []))) (maxHeightGTS [GNode 2 []]))
-- 1 + max (1 + 0) (max (1 + (max (1 + 0) 0)) (maxHeightGTS [GNode 2 []]))
-- 1 + max (1 + 0) (max (1 + (max (1 + 0) 0)) (max (heightGT (GNode 2 [])) (maxHeightGTS [])))
-- 1 + max (1 + 0) (max (1 + (max (1 + 0) 0)) (max (1 + maxHeightGTS []) (maxHeightGTS [])))
-- 1 + max (1 + 0) (max (1 + (max (1 + 0) 0)) (max (1 + 0) (maxHeightGTS [])))
-- 1 + max (1 + 0) (max (1 + (max (1 + 0) 0)) (max (1 + 0) 0))
-- 1 + max (1 + 0) (max (1 + (max (1 + 0) 0)) 1)
-- 1 + max (1 + 0) (max (1 + (max 1 0)) 1)
-- 1 + max (1 + 0) (max (1 + 1) 1)
-- 1 + max (1 + 0) (max 2 1)
-- 1 + max (1 + 0) 2
-- 1 + 2
-- 3

-- heightGT de segunda mano
heightGT' :: GenTree a -> Int
heightGT' (GNode _ []) = 1
heightGT' (GNode x ts) = 1 + maxOfList (map heightGT ts) 0

maxOfList :: [Int] -> Int -> Int
maxOfList [] n = n
maxOfList (x:xs) n = if x > n
                     then maxOfList xs x
                     else maxOfList xs n

-- c) Calcula la imagen especular del árbol.
mirrorGT :: GenTree a -> GenTree a
mirrorGT (GNode x ts) = GNode x (reverse (map mirrorGT ts))

-- d) Retorna una lista con los elementos en el árbol.

-- ***
-- ¿no funciona porque el tipo de retorno es [a] y el de map es [b]?
-- ***
-- toListGT' :: GenTree a -> [a]
-- toListGT' (GNode x []) = [x]
-- toListGT' (GNode x ts) = x : (map toListGT' ts)

toListGT :: GenTree a -> [a]
toListGT (GNode x ts) = x : (toListGTS ts)

toListGTS :: [GenTree a] -> [a]
toListGTS [] = []
toListGTS (t:ts) = toListGT t ++ (toListGTS ts)

-- e) Aplica una función dada a cada elemento en el árbol retornando uno
--    estructuralmente equivalente.
mapGT :: (a -> b) -> GenTree a -> GenTree b
mapGT f (GNode x []) = GNode (f x) []
mapGT f (GNode x ts) = GNode (f x) (map (mapGT f) ts)

-- f) Retorna todos los elementos en un nivel dado del árbol.
levelNGT :: GenTree a -> Int -> [a]
levelNGT (GNode x ts) 0 = [x]
levelNGT (GNode x ts) n = levelNGTS ts (n - 1)

levelNGTS :: [GenTree a] -> Int -> [a]
levelNGTS [] _ = []
levelNGTS (t:ts) 0 = levelNGT t 0 ++ levelNGTS ts 0
levelNGTS (t:ts) n = levelNGT t n ++ levelNGTS ts n

-- Reduction: levelNGT
-- levelNGT (GNode 1 [GNode 2 [], GNode 2 [], GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]]) 2
-- levelNGTS [GNode 2 [], GNode 2 [], GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] (2 - 1)
-- levelNGTS [GNode 2 [], GNode 2 [], GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- levelNGT (GNode 2 []) 1 ++ levelNGTS [GNode 2 [], GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- levelNGTS [] (1-0) ++ levelNGTS [GNode 2 [], GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- levelNGTS [] 1 ++ levelNGTS [GNode 2 [], GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- [] ++ levelNGTS [GNode 2 [], GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- [] ++ levelNGT (GNode 2 []) 1 ++ levelNGTS [GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- [] ++ levelNGTS [] (1 - 1) ++ levelNGTS [GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- [] ++ [] ++ levelNGTS [GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]] 1
-- [] ++ [] ++ levelNGT (GNode 2 [GNode 1 [], GNode 2 [], GNode 3 []]) 1 ++ levelNGTS [] 1
-- [] ++ [] ++ levelNGTS [GNode 1 [], GNode 2 [], GNode 3 []] (1 - 1) ++ levelNGTS [] 1
-- [] ++ [] ++ levelNGTS [GNode 1 [], GNode 2 [], GNode 3 []] 0 ++ levelNGTS [] 1
-- [] ++ [] ++ levelNGT (GNode 1 []) 0 ++ levelNGTS [GNode 2 [], GNode 3 []] 0 ++ levelNGTS [] 1
-- [] ++ [] ++ 1 : [] ++ levelNGTS [GNode 2 [], GNode 3 []] 0 ++ levelNGTS [] 1
-- [] ++ [] ++ [1] ++ levelNGT (GNode 2 []) 0 ++ levelNGTS [GNode 3 []]  0 ++ levelNGTS [] 1
-- [] ++ [] ++ [1] ++ [2] ++ levelNGTS [GNode 3 []]  0 ++ levelNGTS [] 1
-- [] ++ [] ++ [1] ++ [2] ++ levelNGT (GNode 3 []) 0 ++ levelNGTS [] 0 ++ levelNGTS [] 1
-- [] ++ [] ++ [1] ++ [2] ++ [3] ++ levelNGTS [] 0 ++ levelNGTS [] 1
-- [] ++ [] ++ [1] ++ [2] ++ [3] ++ [] ++ levelNGTS [] 1
-- [] ++ [] ++ [1] ++ [2] ++ [3] ++ [] ++ []
-- [] ++ [1] ++ [2] ++ [3] ++ [] ++ []
-- [1] ++ [2] ++ [3] ++ [] ++ []
-- [1, 2] ++ [3] ++ [] ++ []
-- [1, 2, 3] ++ [] ++ []
-- [1, 2, 3] ++ []
-- [1, 2, 3]

-- Un arbolito de ejemplo
gentree :: Num a => GenTree a
gentree = GNode 1 [
                    (GNode 2 [
                              GNode 3 [
                                        GNode 4 []
                                      ]
                             ]),
                    (GNode 2 [
                              GNode 3 [
                                        GNode 4 [
                                                  GNode 5 [
                                                            GNode 6 [],
                                                            GNode 6 []
                                                          ]
                                                ]
                                      ]
                             ]),
                    (GNode 2 [])
                   ]
minigentree = GNode 1 [(GNode 2 []), (GNode 2 []), (GNode 2 [(GNode 1 []), (GNode 2 []), (GNode 3 [])])]

-- 7) Árboles de Bits
-- Llamaremos árbol de bits (BitTree) a una estructura de datos utilizada para
-- representar conjuntos de elementos que pueden ser representados como
-- secuencias de bits (e.g. Int). Cada subárbol izquierdo contiene los elementos
-- cuya representación en secuencia de bits comienza con un cero (Z), mientras
-- que un subárbol derecho contiene los elementos que comienzan con un uno (O).
-- Para verificar si un elemento está contenido en la estructura es necesario
-- recorrer una rama del árbol hasta agotar su representación como secuencia de
-- bits. Si el nodo alcanzado contiene un True, entonces el elemento ésta, en
-- caso contrario no está.

data Bit = Z
         | O
         deriving (Show)

data BitTree = Nil
             | Bit Bool BitTree BitTree
             deriving (Show)

bittree = Bit False
                    (Bit False
                              (Bit False Nil Nil)
                              (Bit True Nil Nil))
                    (Bit True
                              Nil
                              Nil)

-- Tenga en cuenta que la estructura debe respetar el invariante de
-- representación que toda rama debe terminar en un nodo con True (ya que en
-- caso contrario podría reemplazarse con SNil).

-- a) indica si un elemento (representado como una secuencia de bits) está
--    contenido en la estructura.
-- contains :: [Bit] -> BitTree -> Bool
-- contains [] (Bit False _ _) = False
-- contains [] (Bit True Nil Nil) = True
-- contains xs (Bit False z o) = False
-- contains (x:xs) (Bit True z o) =



-- b) inserta un elemento (representado como una secuencia de bits) en un árbol
--    de bits (i.e., contains bs (insert bs SNil) == True).
-- insert :: [Bit] -> BitTree -> BitTree

-- c) elimina un elemento de un árbol de bits (tenga en cuenta que en este caso
--    puede ser necesario restablecer el invariante de representación).
-- remove :: [Bit] -> BitTree -> BitTree

-- d) retorna uno árbol de bits que contiene la unión de los elementos en dos
--    árboles dados.
-- union :: BitTree -> BitTree -> BitTree

-- e) retorna un árbol de bits que contiene la intersección de los elementos en
--    dos árboles dados.
-- intersect :: BitTree -> BitTree -> BitTree



-- Recursión Mutua en tipos

-- data Zig a b = Zig a (Zag a b)
--
-- data Zag a b = Zag b (Zig a b)
