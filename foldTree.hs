data Arbol a = 	Hoja a 
		| Nodo a (Arbol a) (Arbol a) deriving Show

 
foldTree :: (a -> b ) -> (a-> b -> b -> b) -> Arbol a -> b
foldTree f g (Hoja x) = f x
foldTree f g (Nodo y t1 t2) = g y (foldTree f g t1) (foldTree f g  t2)  

sumar3 :: Int -> Int -> Int -> Int
sumar3 a b c = a + b +c

sumArbol :: Arbol Int -> Int
sumArbol = foldTree id (\x s1 s2 -> x + s1 + s2)

sizeArbol :: Arbol a -> Int
sizeArbol = foldTree (const 1) (\n n1 n2 -> 1 + n1 + n2)

hojas :: Arbol a -> Int
hojas = foldTree (const 1) (\x -> (+))

altura :: Arbol a -> Int
altura = foldTree (const 0) (\x n1 n2 -> 1 + max n1 n2) 

ejemplo :: Arbol Int
ejemplo =  Nodo 1 (Nodo 2 (Hoja 3) (Nodo 4 (Hoja 5) (Hoja 6)) ) (Hoja 7)


data ExpA =	Cte Int 
	   	| Sum ExpA ExpA   
		| Mult ExpA ExpA  deriving Show


foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s m (Cte x) = c x
foldExpA c s m (Sum e1 e2) = s (foldExpA c s m e1) (foldExpA c s m e2)
foldExpA c s m (Mult  e1 e2) = m (foldExpA c s m e1) (foldExpA c s m e2)

evalExpA :: ExpA -> Int -- b es Int en el tipado de foldExpA
evalExpA = foldExpA id (+) (*)

simpExpA :: ExpA -> ExpA -- b es ExpA en el tipado de foldExpA
simpExpA = foldExpA Cte armarSuma Mult

armarSuma :: ExpA -> ExpA -> ExpA
armarSuma (Cte 0) e2 = e2
armarSuma e1 (Cte 0) = e1
armarSuma e1 e2 = Sum e1 e2

data A =	B 
		| C A Char A 
		| D A A A 	
		| E A Char 
		| F Int A  deriving Show


foldA :: b -> (b -> Char -> b -> b) -> (b -> b -> b -> b) ->  (b -> Char -> b) ->  (Int -> b -> b) -> A -> b
foldA b fc fd fe ff B = b
foldA b fc fd fe ff (C a1 char a2) = fc (foldA b fc fd fe ff a1) 
			              	char 
					(foldA b fc fd fe ff a2) 

foldA b fc fd fe ff (D a1 a2 a3) = fd	(foldA b fc fd fe ff a1)
					(foldA b fc fd fe ff a2)	
					(foldA b fc fd fe ff a3)

foldA b fc fd fe ff (E a0 char) =  fe	(foldA b fc fd fe ff a0)
					char

foldA b fc fd fe ff (F n a0) =	    ff	n
					(foldA b fc fd fe ff a0)

contarChar :: A -> Int
contarChar = foldA 	0 
			(\cc1 _ cc2 -> 1 + cc1 + cc2) 
			sumar3
			(\cc1 _ -> succ cc1)
			(\_ cc2 -> cc2)

data AB a b = Leaf b | Branch a (AB a b) (AB a b) deriving Show

foldAB :: (b -> c) -> (a -> c -> c -> c) -> AB a b -> c
foldAB fl fb (Leaf x) = fl x --x :: b
foldAB fl fb (Branch y a1 a2) = fb y (foldAB fl fb a1) (foldAB fl fb a2) -- y :: a

bifs :: AB a b -> Int
bifs = foldAB (const 0) (\y n1 n2 ->  1 + n1 + n2)

type AExp = AB BOp Int 
data BOp = Suma | Producto  deriving Show

evalAE :: AExp -> Int
evalAE = foldAB id binOP 

binOP :: BOp -> Int -> Int -> Int
binOP Suma = (+)
binOP Producto = (*)


type Decision s a = AB (s->Bool) a

data Situation = 	Fire 
			| AlienAttack 
			| BossArrive deriving Show
data Action = 	Run
		| Figth
		| Work  deriving Show

--Definamos una función que dada una situación, decida qué acción tomar basada en el árbol

evaluate :: Situation -> Action
evaluate Fire = Run
evaluate AlienAttack = Run
evaluate BossArrive = Work

decide :: Situation -> Decision Situation Action -> Action
decide s = foldAB id (\f a1 a2 -> if (f s)then a1 else a2)

data AG a = GNode a [ AG a ]

sumAG :: AG Int -> Int
sumAG (GNode n ts) = n + sum (map sumAG ts)

--sumAG' :: AG Int -> Int
--sumAG' (GNode n ts) = foldAG0 

--foldAG0' :: 
--foldAG0' f (GNode x ts) = 

foldAG0 :: (a -> [b] -> b) -> AG a -> b
foldAG0 f (GNode x ts) = f x (map (foldAG0 f) ts)


myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) =  f x : myMap f xs


succL :: [Int] -> [Int]
succL = map succ

upperL :: [Char] -> [Char]
upperL = map upper

upper :: Char -> Char
upper 'a' = 'A'
upper x = x



testL :: [Int] -> [Bool]
testL = map (==0) 
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = [] 
myFilter f (x : xs) = if f x
						then x : myFilter f xs
						else myFilter f xs

masQueCero :: [Int] -> [Int]
masQueCero = filter (>0)

digitos :: [Char] -> [Char]
digitos = filter esDigit

esDigit :: Char -> Bool
esDigit 'O' = True
esDigit 'I' = True
esDigit _ = False

noVacias :: [[a]] -> [[a]]
noVacias = filter (not.null)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [] = b
myFoldr f b (x : xs) = f x (foldr f b xs)

sonCincos :: [Int] -> Bool -- dice si todos los elementos son 5
sonCincos [] = True
sonCincos (x : xs) = (\n b -> n == 5 && b) x  (sonCincos xs)

sonCincos' :: [Int] -> Bool
sonCincos' = myFoldr (\n b -> n == 5 && b) True 

cantTotal :: [[a]] -> Int -- dice cuántos elementos de tipo a hay en total
cantTotal [] = 0
cantTotal (xs : xss) = (\ys n -> length ys + n) xs  (cantTotal xss)

cantTotal' :: [[a]] -> Int
cantTotal' = myFoldr (\xs n -> length xs + n) 0

cantTotal'' :: [[a]] -> Int
cantTotal'' xss = sum' (map length xss)

sum' :: [Int] -> Int
sum' = foldr (+) 0

concat' :: [[a]] -> [ a ] -- hace el append de todas las listas en una
concat' [] = []
concat' (xs : xss) = (\xs' ys -> xs' ++ ys) xs (concat xss)

concat'' :: [[a]] -> [ a ] 
concat'' = myFoldr (++) []

--many :: Int -> (a -> a) -> a -> a
many 0 f = id
many n f = f . many (n-1) f

