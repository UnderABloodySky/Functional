data Dir = Left' | Right' | Straight'  deriving Show

data Mapa a = 	Cofre [a]
				| Nada (Mapa a)
				| Bifurcacion [a] (Mapa a) (Mapa a)  deriving Show

--Dar el tipo y definir ​ foldM y ​ recM​ , que expresan los esquemas de recursión
--estructural y primitiva, respectivamente, para la estructura ​ Mapa​ .

data Objeto = Chatarra | Espada | Joya | Monedas Int | Piedra | Oro | Cobre | Medalla deriving Show	
data Criatura = Oso | Duende | Elfo | Enano | Gargola | Dragon | Hada | Bicho deriving Show

object2Criatura :: Objeto -> Criatura
object2Criatura Chatarra = Oso 
object2Criatura Espada = Duende
object2Criatura Joya = Elfo
object2Criatura (Monedas _) = Enano 
object2Criatura Piedra = Gargola
object2Criatura Oro = Dragon
object2Criatura Cobre = Hada
object2Criatura Medalla = Bicho

dirAEspada :: [Dir]
dirAEspada = [Right', Straight', Right', Straight']

ruta :: [Dir]
ruta = [Left', Straight', Right' ]

ejemplo:: Mapa Objeto 
ejemplo = 
		Bifurcacion [Monedas 8] 
				(Nada (Bifurcacion [Chatarra] (Cofre [Oro]) 
									  (Cofre [Cobre, Piedra]) ))
				(Nada (Bifurcacion [Cobre] (Cofre [Chatarra, Chatarra, Oro]) 
									  (Nada (Cofre 
									  			[Espada])))) 

esPiedra :: Objeto -> Bool
esPiedra Piedra = True
esPiedra _ = False


esOro :: Objeto -> Bool
esOro Oro = True
esOro _ = False

esJoya :: Objeto -> Bool
esJoya Joya = True
esJoya _ = False

esEspada :: Objeto -> Bool
esEspada Espada = True
esEspada _ = False

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM c n b (Cofre os) = c os
foldM c n b (Nada map) = n (foldM c n b map)
foldM c n b (Bifurcacion os map1 map2) = b os (foldM c n b map1) (foldM c n b map2)

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> b -> Mapa a -> b -> b) -> Mapa a -> b
recM cr nr br (Cofre os) = cr os
recM cr nr br (Nada map) = nr map (recM cr nr br map)
recM cr nr br (Bifurcacion os map1 map2) = br os map1 (recM cr nr br map1) map2 (recM cr nr br map2)

objects :: Mapa a -> [a]​ 
--que describe la lista de todos los objetos presentes en el mapa dado.
objects = foldM id id (\xs l1 l2 -> xs ++ l1 ++ l2)

mapM :: (a -> b) -> Mapa a -> Mapa b
--Transforma los objetos del mapa dado aplicando la función dada.
mapM f = foldM (Cofre . map f) id (Bifurcacion . map f)

has :: (a -> Bool) -> Mapa a -> Bool​ 
-- que indica si existe algún objeto asque cumpla con la condición dada en el mapa dado.
has f = foldM (any f) id (\os b1 b2 -> any f os || b1 || b2)

has' :: (a -> Bool) -> Mapa a -> Bool​
has' f (Cofre os) =  any f os
has' f (Nada mapa) = has mapa
has' f (Bifurcacion os mapa1 mapa2) = any f os || has' f mapa1 || has' f mapa2



hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool​ 
--que indica si un objeto al final del camino dado cumple con la condición dada en el mapa dado. 
hasObjectAt f =	foldM c n b
	where c os [] = any f os
	      c _  _  = error "No path"
	      n r  []  = False
	      n r (Straight' : ds) = r ds
	      n r _ = error "No way"
	      b os r1 r2 [] = any f os
	      b _  r1 r2 (Left' : ds) = r1 ds
	      b _  r1 r2 (Right' : ds) = r2 ds
	      b _  r1 r2 _ = error "No avaliable way" 

heigthM :: Mapa a -> Int
heigthM = foldM (const 1) id (\_ n1 n2 -> succ (max n1 n2))

isLongest :: Mapa a -> Mapa a -> Bool
isLongest m1 m2 = heigthM m1 > heigthM m2

longestPath :: Mapa a -> [Dir]
--que describe el camino más largo en el mapa dado.
longestPath = foldM (const []) (Straight' :) (\x r1 r2 -> if length r1 > length r2
																then Left' : r1
																else Right' : r2)

objectsOfLongestPath :: Mapa a -> [a]​ 
--Que describe lalista con los objetos presentes en el camino más largo del mapa dado.
objectsOfLongestPath = recM id (const id) (\xs t1 l1 t2 l2 -> if isLongest t1 t2
															then xs ++ l1
															else xs ++ l2)

allPaths :: Mapa a -> [[Dir]]​ 
--que describe la lista con todos los caminos del mapa dado.
allPaths = foldM (const [[]]) (map (Straight' :)) (\_ l1 l2 -> map (Left' :) l1 ++  map (Right' :) l2)

singular :: a -> [a]
singular x = [x]

objectsPerLevel :: Mapa a -> [[a]]​ 
--que describe la lista con todos los objetos por niveles del mapa dado.
objectsPerLevel = foldM singular (\ lr -> [] : lr) (\xs l1 l2 -> xs : appendLevels l1 l2 )

objectsPerLevel'  :: Mapa a -> [[a]]
objectsPerLevel' (Cofre os) = singular os
objectsPerLevel' (Nada mapa) = [] : objectsPerLevel' mapa
objectsPerLevel' (Bifurcacion os mapa1 mapa2) = os : (objectsPerLevel' mapa1) (objectsPerLevel' mapa2)

appendLevels :: [[a]] -> [[a]] -> [[a]]
appendLevels [] [] = []
appendLevels xss [] = xss
appendLevels [] yss = yss
appendLevels (xs:xss) (ys:yss) = (xs ++ ys) : appendLevels xss yss

-- :[] a lo que le pasan lo pone entre [] (\x -> [x])
-- [] : agrega una lista vacia a lo que le pasen (\r -> [] : r)