-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------- ExpA -------------------------------------------------------------- 
-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving Show

--Dar el tipo y definir ​ foldExpA​ , que expresa el esquema de recursión
--estructural para la estructura ​ ExpA

--ejemplo = Suma (Suma (Cte (88)) (Cte 12)) (Prod (Cte 64) (Suma (Cte 16) (Cte 42)))
foldExpA​ :: (Int -> b) -> (b -> b ->b) -> (b -> b -> b) -> ExpA -> b
foldExpA​ fc fs fp (Cte n) = fc n
foldExpA​ fc fs fp (Suma e0 e1) = fs (foldExpA fc fs fp e0) (foldExpA fc fs fp e1)
foldExpA​ fc fs fp (Prod e0 e1) = fp (foldExpA fc fs fp e0) (foldExpA fc fs fp e1)

unoSiEsCero :: Int -> Int
unoSiEsCero 0 = 1
unoSiEsCero _ = 0

suma3 :: Int -> Int -> Int -> Int
suma3 x y z = x + y + z

cantidadDeCeros :: ExpA -> Int​
cantidadDeCeros = foldExpA​ unoSiEsCero (+) (+)

esNegativo :: Int -> Bool​
esNegativo n = n < 0

noTieneNegativosExplicitosExpA :: ExpA -> Bool​
-- que describe si la expresión dada no tiene números negativos de maneraexplícita.
noTieneNegativosExplicitosExpA = foldExpA​ 
											(not . esNegativo)
											(&&)
											(&&)

armarSum :: ExpA -> ExpA -> ExpA
armarSum (Cte 0) e2 = e2
armarSum e1 (Cte 0) = e1
armarSum e1 e2 = Suma e1 e2

armarProd :: ExpA -> ExpA -> ExpA
armarProd (Cte 1) e2 = e2 
armarProd e1 (Cte 1) = e1
armarProd e1 e2 = Prod e1 e2

simplificarExpA' :: ExpA -> ExpA​
-- describe una expresión con el mismo significado que la dada, pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución
-- debe ser exclusivamente ​ simbólica . ​
simplificarExpA' = foldExpA​		Cte
								armarSum
								armarProd 

evalExpA' :: ExpA -> Int​
--que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA' = foldExpA​ id (+) (*)

showExpA :: ExpA -> String​ 
--que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showExpA = foldExpA​ show (\ss1 ss2 ->"(" ++ ss1 ++ "+" ++ ss2 ++ ")") (\sp1 sp2 -> "(" ++ sp1 ++ "*" ++ sp2 ++ ")")


-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------- EA ---------------------------------------------------------------- 
-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

--Ejercicio 2)
--Dada la definición de ​ EA​ :
data EA = Const Int | BOp BinOp EA EA deriving Show
data BinOp = Sum | Mul deriving Show 

ejemplo2 = BOp Sum (BOp Mul (Const 1)  (Const 4)) 
				(BOp Sum (BOp Sum
							(BOp Sum (Const 7) (Const 0)) (BOp Mul (Const 1) (Const 42))) 
							(BOp Sum (Const 0) (Const 0)))
-- Dar el tipo y definir ​ foldEA​ , que expresa el esquema de recursión estructural para la estructura ​ EA​ 
foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA​ fc fb (Const n) = fc n
foldEA​ fc fb (BOp b e1 e2) = fb b (foldEA​ fc fb e1) (foldEA​ fc fb e2)

--Resolver las siguientes funciones utilizando ​ foldEA​ :
noTieneNegativosExplicitosEA :: EA -> Bool​ 
--describe si la expresión dada no tiene números negativos de manera
--explícita.
noTieneNegativosExplicitosEA = foldEA​ (not . esNegativo) (\_ b1 b2 -> b1 && b2) 

armarBOp :: BinOp -> EA -> EA -> EA
armarBOp Sum (Const 0) e = e
armarBOp Sum e (Const 0) = e
armarBOp Mul (Const 1) e = e
armarBOp Mul e (Const 1) = e
armarBOp bop e1 e2 = BOp bop e1 e2

simplificarEA' :: EA -> EA​
-- Que describe una expresión con el mismo significado que la dada, pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0.
-- La resolución debe serexclusivamente simbólica.
simplificarEA' = foldEA​ Const armarBOp

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp Sum = (+)
evalBinOp Mul = (*)

evalEA' :: EA -> Int​ 
--Describe el número que resulta de
--evaluar la cuenta representada por la expresión aritmética dada.
evalEA' = foldEA​ id evalBinOp

showBinOp :: BinOp -> String​ -> String​ -> String​
showBinOp Sum str1 str2 = "(" ++ str1 ++ " + " ++ str2 ++ ")"
showBinOp Mul str1 str2 = "(" ++ str1 ++ " * " ++ str2 ++ ")"

showEA' :: EA -> String​ 
--Que describe el string sin espacios y con paréntesis correspondiente a la expresión dada. 
showEA' = foldEA​ show showBinOp 

binOp2ExpA :: BinOp -> ExpA -> ExpA -> ExpA
binOp2ExpA Sum e1 e2 = Suma e1 e2
binOp2ExpA Mul e1 e2 = Prod e1 e2

ea2ExpA' :: EA -> ExpA​
-- que describe una expresión aritmética representada con el tipo ExpA, cuyo significado es el mismo que la
-- dada.
ea2ExpA' = foldEA​ Cte binOp2ExpA 

data Arbol a b = Leaf b | Branch a (Arbol a b) (Arbol a b) deriving Show

armarArbol :: BinOp -> Arbol BinOp Int -> Arbol BinOp Int -> Arbol BinOp Int 
armarArbol bOp t1 t2 = Branch bOp t1 t2

ea2Arbol' :: EA -> Arbol BinOp Int​ 
--que describe la representación como elemento del tipo ​ Arbol BinOp Int de la expresión aritmética dada.
--Demostrar que ​ evalEA' es equivalente a ​ evalEA (ejercicio 1.a.i de la práctica 9).
ea2Arbol' = foldEA​ Leaf armarArbol


-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------- Tree ---------------------------------------------------------------- 
-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------


--Dada la definición de ​ Tree​ :

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT b fn EmptyT = b
foldT​ b fn (NodeT x t1 t2) = fn x (foldT​ b fn t1) (foldT​ b fn t2)

--Definir las siguientes funciones utilizando ​ foldT​ :
mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT​ EmptyT (\a t1 t2 -> NodeT (f a) t1 t2)

sumT :: Tree Int -> Int
sumT = foldT​ 0 suma3

sizeT :: Tree a -> Int
sizeT = foldT​ 1 (\ _ n1 n2 -> suma3 1 n1 n2)

heightT :: Tree a -> Int
heightT = foldT​ 0 (\ _ n1 n2 -> succ n1 + n2)

preOrder :: Tree a -> [a]
preOrder = foldT​ [] (\ x l1 l2 -> [x] ++ l1 ++ l2)

inOrder  :: Tree a -> [a]
inOrder = foldT​ [] (\ x l1 l2 -> l1 ++ [x] ++ l2)

postOrder :: Tree a -> [a]
postOrder = foldT​ [] (\ x l1 l2 -> l1 ++ l2 ++ [x])

mirrorT :: Tree a -> Tree a
mirrorT  = foldT​ EmptyT (\x t1 t2 -> NodeT x t2 t1)

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT 0 (\x n1 n2 -> if f x
										then succ n1 + n2
										else n1 + n2)​

--	   0
--	 /   \
--  1     4
-- / \   / \
--2   3 5   6
--         / \
--        7   8

ejemplo3 :: Tree Int
ejemplo3 = NodeT 0 (NodeT 1 
							(NodeT 2 EmptyT EmptyT)
							(NodeT 3 EmptyT EmptyT))
				   (NodeT 4 
				   			(NodeT 5 EmptyT EmptyT)
				   			(NodeT 6 (NodeT 7 EmptyT EmptyT)
				   			   		 (NodeT 8 EmptyT EmptyT)))

--	   6
--	 /   \
--  2     8
-- / \   / \
--1   3 7   9

ejemploBST = NodeT 6 (NodeT 2 
							(NodeT 1 EmptyT EmptyT)
							(NodeT 3 EmptyT EmptyT))
					(NodeT 8 
							(NodeT 7 EmptyT EmptyT)
							(NodeT 9 EmptyT EmptyT))
			  
--	[6 2 1] [6 2 3] [6,8,7] [6,8,9]

agregarPrimero :: a -> ([a], [a]) -> ([a], [a])
agregarPrimero x (xs, ys) = ((x:xs), ys)

agregarSegundo :: a -> ([a], [a]) -> ([a], [a])
agregarSegundo x (xs, ys) = (xs, (x:ys))

hacerUno :: ([a], [a]) -> ([a], [a]) -> ([a], [a])
hacerUno (xs, ys) (xs', ys') = (xs ++ xs' , ys ++ ys') 

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT​ ([], []) (\x p p' -> if f x
											then agregarPrimero x (hacerUno p p')
 											else agregarSegundo x (hacerUno p p'))

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT (const EmptyT) (\x fr1 fr2 t2 -> case t2 of
											EmptyT -> EmptyT
											(NodeT y t1' t2') -> NodeT (f x y) (fr1 t1') (fr2 t2')) 

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] (\x l1 l2 -> if length l1 > length l2
													then x : l1
													else x : l2)

agregar :: a -> [[a]] -> [[a]]
agregar x = map (x:)  

miniejemplo :: Tree Int
miniejemplo = NodeT 2 (NodeT 4 EmptyT EmptyT) 
					  (NodeT 6 EmptyT EmptyT)
					  			
isLeaf :: Tree a -> Bool
isLeaf EmptyT = True
isLeaf _ = False

todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
todosLosCaminos = recT [[]] (\x t1 l1 t2 l2 -> if isLeaf t1 && isLeaf t2 
														then [[x]]
														else map (x:) (l1 ++ l2))
--todosLosCaminos = foldT [[]] (\x l1 l2 -> map (x:) l1 ++ map (x:) l2) Repetidos
--todosLosCaminos = foldT [[]] (\x l1 l2 -> agregar x (l1 ++ l2))

--allPaths = foldM (const [[]]) (map (Straight' :)) (\_ l1 l2 -> map (Left' :) l1 ++  map (Right' :) l2

isEmpty :: Tree a -> Bool
isEmpty EmptyT = True
isEmpty _ = False

paths :: Tree a -> [[a]]
paths (EmptyT) = [[]]
paths (NodeT x EmptyT EmptyT) = [[x]]
paths (NodeT x left right) = map (x:) (paths left ++ paths right)

--todosLosNiveles :: Tree a -> [[a]]
--todosLosNiveles = foldT [[]] (\x l1 l2 -> l1 ++ l2)

nivelN :: Tree a -> Int -> [a]
nivelN = foldT (const []) (\x fr1 fr2 m -> if m == 0
												then [x]
												else fr1 (m-1) ++ fr2 (m-1))

--Dar el tipo y definir la función ​ recT​ , que expresa el esquema de recursión
--primitiva para la estructura ​ Tree​ .

recT :: b -> (a -> Tree​ a -> b -> Tree​ a -> b -> b) -> Tree​ a -> b
recT b f EmptyT = b
recT b f (NodeT x t1 t2) = f x t1 (recT b f t1) t2 (recT b f t2) 

--Definir las siguientes funciones utilizando ​ recT​ :

insertT :: Ord a =>  a -> Tree a -> Tree a​ 
--que describe el árbolresultante de insertar el elemento dado en el árbol dado, teniendo en
--cuenta invariantes de BST.
insertT x = recT EmptyT (\x' t1 r1 t2 r2 ->  if x < x'
												then NodeT x r1 t2
												else NodeT x' t1 r2)

insertT' :: Ord a =>  a -> Tree a -> Tree a​ 
--que describe el árbolresultante de insertar el elemento dado en el árbol dado, teniendo en
--cuenta invariantes de BST.
insertT' x = recT f g
		where 
			f = NodeT x EmptyT EmptyT
			g x' t1 r1 t2 r2 =  if x < x'
									then NodeT x r1 t2
									else NodeT x' t1 r2

caminoHasta' :: Eq a => a -> Tree a -> [a]
caminoHasta' x = recT f g 
	where 
		f = [] 
		g x' t1 l1 t2 l2 = if x == x'
								then [x]
								else if any (==x) l1
											then x' : l1
											else x' : l2

--Ejercicio 4)
--Dada la siguiente definición:
type Record a b = [(a,b)]

ejemploRec :: [Record Int Bool]
ejemploRec = [[(1, True), (2, False), (3, False), (4, True)], 
			 [(5, True), (6, False)], [(7,True)],
             [],
             [(8,False)],
             [(9, True), (10, True), (11, True), (12, True)],
             [(32, True), (16, True), (8, False), (42, True)]]


ejemploRec2 :: [Record Int Bool]
ejemploRec2 = [[(1, True), (3, False)], 
			 [(5, True), (6, False)], [(7,True)],
             [(8,False)],
             [(8, False), (42, True)]]



ejemplito :: Record Int Bool
ejemplito = [(2,True), (1, False)]


--Definir las siguientes funciones sin utilizar recursión estructural explícita:

-- El Registro ES una lista
-- Ejemplo de Registro = [(a1,b1), (a2,b2)] , [(a3,b3)]
-- Ejemplo de Lista de registros [(a0, b0)], [(a1,b1), (a2,b2)] , [(a3,b3)], [],  [(a4,b4)], [(a5,b5), (a6, b6)]

select :: (Record a b -> Bool) -> [Record a b] -> [Record a b]​ 
--que a partir de la lista de registros dada describe la lista de los registros que cumplen con la condición dada.
select = filter 

project :: (a -> Bool) -> [Record a b] -> [Record a b]​ 
--, que a partir de la lista de registros dada describe la lista de registros solo con los campos que cumplen la condición dada.
project f = map (filter (f . fst))

conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool​
--que describe el predicado que da True solo cuando los dos predicados dados lo hacen.
conjunct = \f g x-> (&&) (f x) (g x) 

zipWithl :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithl = \f -> foldr (\x fr xs -> case xs of
										[] -> []
										(x':xs') -> f x x' : fr xs') (\_ -> [])

crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]​ 
--que describe el resultado de aplicar una función a cada elemento del producto cartesiano de las dos listas de registros dadas.
crossWith = zipWithl

singular :: a -> [a]
singular x = [x]

product' :: [Record a b] -> [Record a b] -> [Record a b]​ 
-- que describe la lista de registros resultante del producto cartesiano combinado de las dos listas de registros dadas. Es decir, la unión de los
-- campos en los registros del producto cartesiano.
product' rxs rys = foldr (\xs r -> map (xs++) rys ++ r) [] rxs 

r0 :: Record String​ String​
r0 = [("nombre", "Pedro"), ("ocupacion", "Empleado")]

r1 :: Record String​ String​
r1 = [("edad", "16")]

r2 :: Record String​ String​
r2 = [("nombre", "Josefina"), ("estudios", "terciarios"), ("algo", "otra cosa")]

r3 :: Record String​ String​
r3 = []

r4 :: Record String​ String​
r4 = [("DNI", "3333333"), ("Ejem", "aham"), ("pelicula", "killbill"),("otra cosa", "algo")]

a :: [Record String String]
a = [r0, r1] 

b :: [Record String String]
b = [r2, r3 , r4]



recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)  

--similar :: Eq a => Record a b -> Record a b​ 
--que describe el registro resultante de descartar datos cuyos campos sean iguales.
--similar = filter (\p -> not . elem p)


--[(A), (B)]] [[(C),(D),(E)]]

data Query a b = Table [Record a b]
				| Product (Query a b) (Query a b)
				| Projection (a -> Bool) (Query a b)
				| Selection (Record a b -> Bool) (Query a b)

foldQ :: ([Record a b]->c) -> (c -> c -> c) -> ((a -> Bool) -> c -> c) ->  ((Record a b -> Bool) -> c ->  c) -> Query a b -> c
foldQ t pr pj s (Table rgs) = t rgs
foldQ t pr pj s (Product q1 q2) = pr (foldQ t pr pj s q1) (foldQ t pr pj s q2) 
foldQ t pr pj s (Projection f q) = pj f (foldQ t pr pj s q) 
foldQ t pr pj s (Selection f q) = s f (foldQ t pr pj s q) 

--ejemploQ = Projection (/= "age")
--					(Selection
--						(\xs -> any c /= "name" || v == "Edward Snowden")
--						(Table [ [("name", "Edward Snowden"),("age", "29")]
--					[("name", "Jason Bourne"), ("age", "40")] ]))

--Definir las siguientes funciones sin utilizar recursión explícita:

dropFst :: a -> b -> b
dropFst x y = y

tables :: Query a b -> [[Record a b]]​ 
--Describe la lista de todas las tablas involucradas en la query dada.
tables = foldQ singular --Igual a (:[])
			   (++)
			   dropFst
			   dropFst

execute :: Query a b -> [Record a b]
--que describe el --resultado de ejecutar la query dada.
execute = foldQ id
				product'
				project
				select

--compact :: Query a b -> Query a b
--que describe la query resultante de compactar las selecciones y proyecciones consecutivas
--en la query dada.



--Ejercicio 6) Dadas las siguientes definiciones para representar mapas con diferentes
--puntos de interés que pueden presentar objetos:

data Dir = Left' | Right' | Straight'

data Mapa a =   Cofre [a]
				| Nada (Mapa a)
				| Bifurcacion [a] (Mapa a) (Mapa a)


--Dar el tipo y definir ​ foldM y ​ recM​ , que expresan los esquemas de recursión
foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb (Cofre objs) = fc objs
foldM fc fn fb (Nada mapa) = fn (foldM fc fn fb mapa)
foldM fc fn fb (Bifurcacion objs m1 m2) = fb objs (foldM fc fn fb m1) (foldM fc fn fb m2)

recM :: ([a] -> b) -> (Mapa a -> b-> b) -> ([a] -> Mapa a -> b -> Mapa a -> b -> b) -> Mapa a -> b  
recM fc fn fb (Cofre objs) = fc objs 
recM fc fn fb (Nada mapa) = fn mapa (recM fc fn fb mapa)
recM fc fn fb (Bifurcacion objs m1 m2) = fb objs m1 (recM fc fn fb m1) m2 (recM fc fn fb m2)

--Definir las siguientes funciones sin utilizar recursión explícita:


objects :: Mapa a -> [a]
--que describe la lista de todos los objetos presentes en el mapa dado.
objects = foldM id id (\ xs r1 r2 -> xs ++ r1 ++ r2)

mapM :: (a -> b) -> Mapa a -> Mapa b​ 
-- que transforma los objetos del mapa dado aplicando la función dada.
mapM f = foldM (Cofre . map f) id (Bifurcacion . map f)

has :: (a -> Bool) -> Mapa a -> Bool​ 
--que indica si existe algún objeto que cumpla con la condición dada en el mapa dado.
has f = foldM (any f) id (\xs b1 b2 -> any f xs || b1 || b2)

hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool​
--que indica si un objeto al final del camino dado cumple con la condición dada en el mapa dado.
hasObjectAt p = foldM f g h
    where f xs [] = any p xs
          f _ _ = False
          g r (Straight':ds) = r ds
          g _ _ = False
          h xs r1 r2 [] = any p xs
          h xs r1 r2 (Left':ds) = r1 ds
          h xs r1 r2 (Right':ds) = r2 ds
          h _ _ _ _ = False
