data Ft a = Empty | Leaf a | Branch (Ft a) (Ft a) | Nested (Ft(Ft a))
data NList a = EmptyL | Cons (a, NList (a,a))

-- foldtFt :: b -> (a -> b) -> (b -> b -> b) -> (a -> b) -> Ft a -> b
-- foldtFt z l b n Empty = z b 
-- foldtFt z l b n (Leaf x) = l x
-- foldtFt z l b n (Branch t1 t2) = b (foldtFt z l b n t1) (foldtFt z l b n t2)
-- foldtFt z l b n (Nested t1) = n (foldtFt z l b n t1)


m0 = Just(Just (Just (Just Nothing)))

m1 = Just Nothing 

m2 = Just(Just Nothing)


putJ m = Just m
