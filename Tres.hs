
doble :: Int -> Int
doble x = x + x
compose f g x = f (g x)

-- Ejercicio 1) Definir funciones

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x, y) = f x y

-- Ejercicio 2) Reescribir las siguientes definiciones sin utilizar ​ where​ , ​ let o lambdas, y
-- utilizando la menor cantidad de paréntesis posible.
-- Ejercicio 3) Indicar el tipo de cada una de las funciones del ejercicio anterior,
-- utilizando también la menor cantidad posible de paréntesis.

-- apply f = g
-- 	where g x = f x 			\f -> \x -> f x
-- apply' ::  (a -> b) -> a -> b 
apply' f x = f x

-- twice f = g
-- 	where g x = f (f x)			\f -> \x -> f (f x)
-- twice' :: (a -> a) -> a -> a
twice' f = f . f

-- id = \x -> x
-- id' :: a -> a
id' x = x

-- flip f = g
-- 	where g x = h
-- 		  h y = (f y) x
--flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

swap :: (a, b) -> (b, a)
swap (x, y) = (y ,x)

-- uflip f = g
-- 	where g p = f (swap p)
--uflip' ((a,b) -> c) -> (b, a) -> c 
uflip' f p = f (swap p)

-- const = \x -> (\y -> x)
const' :: a -> b -> a
const' x y = x 

-- compose = \f -> (\g -> (\x -> f (g x)))
compose' :: (a -> b) -> (c -> a) -> c -> b 
compose' f g = f . g


-- Ejercicio 4) En las expresiones que siguen, colocar los paréntesis que están
-- implícitos, manteniendo el significado de cada una de las expresiones, y dar el tipo
-- de cada una de ellas, suponiendo dadas las definiciones de los ejercicios anteriores.

-- apply apply apply
-- ((apply apply) apply) :: (a ->b) -> a -> b

-- twice doble 2
-- ((twice doble) 2) :: Int

-- twice twice twice swap
-- (((twice twice) twice) swap) :: (a,a) -> (a,a)

-- flip twice 1 doble
-- (((flip twice) 1) doble) :: Int


-- Ejercicio 5)
-- Reescribir las siguientes definiciones utilizando sólo lambdas (sin ​ where ni ​ let​ ).

-- appDist f = g
-- 	where g (x, y) = (f x, f y)
appDist' = \f (x,y) -> (f x, f y)

-- subst f = h
-- 	where h g = k
-- 		where k x = (f x) (g x)
subst' = \f g x -> f x (g x)

-- appDup f = g
-- 	where g x = f (x, x)
appDup' = \f x -> f (x,x)

-- appFork (f, g) = h
-- 	where h x = (f x, g x)
appFork' = \(f, g) x -> (f x, g x)

-- appPar​ ​ (f, g) = h
-- where h (x, y) = (f x, g y)
appPar' = \(f, g) (x,y) -> (f x, g y) 


-- Ejercicio 6) Indicar cuáles de las siguientes expresiones tienen tipo según el sistema
-- de tipos de Hindley Milner. En el caso de que alguna sea incorrecta, ¿existirá una
-- expresión que utilice las mismas partes, pero asociadas de forma diferente y que sí
-- tenga significado? En el caso de que sí, escribir tal variante.

-- compose (fst snd) 
-- X 		~		compose fst snd :: (a, (b,c)) -> b

-- (uncurry curry snd)
-- X 		~		uncurry (curry snd) :: (a, b) -> b

apply f x = f x
-- (apply id) ((id apply) apply)
-- OK :: (a -> b) -> a -> b

-- compose (compose doble doble)
-- OK :: (a -> Int) -> a -> Int

-- (compose compose) doble doble
-- X 		~		compose (compose doble doble) :: (a -> Int) -> a -> Int 


-- Ejercicio 7) Dada la siguiente definición, indicar cómo podría reescribirse usando
-- compose​ y ​ id​ :
many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

many' :: Int -> (a -> a) -> a -> a
many' 0 _ = id
many' n f = f . many' (n-1) f


-- Ejercicio 8) Quitar de los siguientes tipos la mayor cantidad de paréntesis posible, sin
-- cambiar su significado. En cada caso, escribir en castellano cómo debería leerse el
-- tipo obtenido de forma correcta, y cómo con la convención de leerla como si
-- estuviera no-currificada.
-- Por ejemplo, para ​ Int -> (Int -> Int) las respuestas serían: ​ Int -> Int ->
-- Int, ​ en castellano en forma correcta ​ “es una función que toma un entero y
-- devuelve una función que toma otro entero y devuelve un entero” ​ , y en castellano
-- usando la convención “​ es una función que toma dos enteros y devuelve un entero” . ​

-- (Int -> Int) -> (Int -> Int)
-- (Int -> Int) -> Int -> Int

-- (a -> (b -> c)) -> (a -> b) -> c
-- (a -> b -> c) -> a -> b -> c

-- (a -> b, c -> d) -> ((a, c) -> (b, d))
-- (a -> b, c -> d) -> (a, c) -> (b, d)

-- ((a, a) -> b) -> (a -> b)
-- ((a, a) -> b) -> a -> b

-- (a -> (b -> c)) -> (b -> (a -> c))
-- (a -> b -> c) -> b -> a -> c

-- (a -> b) -> ((a, a) -> (b, b))
-- (a -> b) -> (a, a) -> (b, b)

-- (​ a -> b, a -> c) -> (a -> (b, c))
-- (a -> b, a -> c) -> a -> (b, c)

-- (a -> (b -> c)) -> ((a -> b) -> (a -> c))
-- (a -> b -> c) -> (a -> b) -> a -> c

-- a -> (b -> a)
-- a -> b -> a


-- Ejercicio 9) Dar expresiones equivalentes a las funciones definidas a continuación
-- utilizando funciones como ​ compose​ , ​ flip​ , etc. (dadas en los ejercicios anteriores) y
-- sin utilizar lambas.
twice f = f .f 

cuadruple0 x = doble (doble x)
cuadruple1 = doble . doble
cuadruple2 = many 2 doble
cuadruple3 = twice doble

timesTwoPlusThree0 x = (+) (doble x) 3
timesTwoPlusThree1 x = ((+) .doble) x 3
timesTwoPlusThree2 = flip ((+) .doble) 3
timesTwoPlusThree3 =  (+3) . doble

fourTimes0 f x = f (f (f (f x)))
fourTimes1 f x = many 4 f x 
fourTimes2 f x = twice f (twice f x)
fourTimes3 f x = twice twice f x