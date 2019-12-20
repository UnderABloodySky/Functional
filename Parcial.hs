-- Parcial

data PredExp a = Pred (a -> Bool) | Not (PredExp a) | LOp LogicOp (PredExp a) (PredExp a)
data SetExp a = Empty | Unit a | Union (SetExp a) (SetExp a)

data LogicOp = AND | OR

evalPE :: PredExp a -> a -> Bool
evalPE (Pred f) = f
evalPE (Not p) = liftB not (evalPE p)
evalPE (LOp lop p1 p2) =  liftB2 (evalLogicOp lop) (evalPE p1) (evalPE p2)

liftB2:: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> a -> Bool
liftB2 f g h x = f (g x) (h x)

liftB :: (Bool -> Bool) -> (a -> Bool) -> a -> Bool
liftB f g x = f (g x)

sExp2pExp :: SetExp a -> Pred a
sExp2pExp Empty = Pred (const False)
sExp2pExp (Unit x) = Pred (x==)
sExp2pExp (Union s1 s2) = LOp OR (sExp2pExp s1) (sExp2pExp s2)

evalSE :: SetExp a -> a -> Bool
evalSE Empty = const False
evalSE (Unit x) = (x==)
evalSE (Union s1 s2) = liftB2 (||) (evalSE s1) (evalSE s2)

mapPE :: (a -> b) -> PredExp b -> PredExp a
mapPE f (Pred g) = Pred (f.g)
mapPE f (Not p) = Not (mapPE f p)
mapPE f (LOp lop p1 p2) = LOp lop (mapPE f p1) (mapPE f p2)

foldPE :: ((a -> Bool) -> b) -> (b -> b) -> (LogicOp -> b -> b -> b) -> PredExp a -> b
foldPE p n l (Pred f) = p f
foldPE p n l (Not p) = n (foldPE p n l p)
foldPE p n l (LOp lop p1 p2) = l lop (foldPE p n l p1) (foldPE p n l p2)

evalPE' :: PredExp a -> a -> Bool
evalPE' = foldPE id (\f -> liftB not f) (\lop f1 f2 -> liftB2 (evalLogicOp lop) f1 f2)  

mapPE' :: (a -> b) -> PredExp b -> PredExp a
mapPE' = \f -> foldPE (\g -> Pred (g . f)) Not LOp

foldSE :: ((a -> Bool) -> b) -> (b -> b) -> (LogicOp -> b -> b -> b) -> PredExp a -> b
foldSE e ut un Empty = e
foldSE e ut un (Unit x) = ut x
foldSE e ut un (Union p1 p2) = un (foldPE e ut un s1) (foldPE e ut un s2)

sExp2pExp' :: SetExp a -> Pred a
sExp2pExp' = foldSE (Pred (const False)) (Pred . (==)) (LOp OR) 

evalSE' :: SetExp a -> a -> Bool
evalSE' = foldSE (const False) (==) (liftB2 (||))