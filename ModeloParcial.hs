data Component = Cargo | Engine | Shield | Cannon deriving Show

data Spaceship = Module Component Spaceship Spaceship 
				| Plug deriving Show

data Direction = Larboard | Starboard

data Size = Small | Big | Torpedo

type Hazard = (Direction, Int, Size)

data Direction = Larboard | Starboard

data Size = Small | Big | Torpedo

type Hazard = (Direction, Int, Size)

isShield :: Component -> Bool
isShield Shield =True
isShield _ = False

isCannon :: Component -> Bool
isCannon Cannon = True
isCannon _ = False

shielded :: Spaceship -> Bool
--que indica si la nave posee al menos un generador de campos de fuerza.
shielded Plug = False
shielded (Module comp s1 s2) = isShield comp || shielded s1 || shielded s2

armed :: Spaceship -> Bool
--que indica si la nave posee al menos un cañón.
armed Plug = False
armed (Module comp s1 s2) = isCannon comp || armed s1 || armed s2


oneIfIsEngine :: Component -> Int
oneIfIsEngine Engine = 1
oneIfIsEngine _ = 0

thrust :: Spaceship -> Int
--que retorna el poder de propulsión de una nave.
thrust Plug = 0
thrust (Module comp s1 s2) = if isPlug s1 && isPlug s2
										then oneIfIsEngine comp
										else thrust s1 + thrust s2   

--wreck :: Hazard -> Spaceship -> Spaceship
--que devuelve la nave resultante de desprender los módulos dependientes del módulo donde se recibe un impacto
--(esta función asume que se produce el impacto).


foldSS :: b -> (Component -> b -> b -> b) -> Spaceship -> b
foldSS p m Plug = p
foldSS p m (Module comp s1 s2) = m comp (foldSS p m s1) (foldSS p m s2)

recSS :: b -> (Component -> Spaceship -> b -> Spaceship -> b -> b) -> Spaceship -> b
recSS p m Plug = p
recSS p m (Module comp s1 s2) = m comp s1 (recSS p m s1) s2 (recSS p m s1) 

oneIfIsModule :: Component -> Int
oneIfIsModule Cargo = 1
oneIfIsModule _ = 0

capacity :: Spaceship -> Int
--que retorna la capacidad de la nave, donde cada módulo de carga aporta una unidad de capacidad.
capacity = foldSS 0 (\comp n1 n2 -> oneIsModule comp + n1 + n2)

largest :: [Spaceship] -> Spaceship
--que dada una lista de naves retorna una de capacidad máxima.
largest = foldr1 (max)
--dimensions :: Spaceship -> (Int,Int)
--que dada una nave retorna su alto y ancho (pensando el alto como la cantidad de componentes de la rama más
--larga y el ancho como la cantidad de componentes del nivel más ancho).

--manoeuvre :: Spaceship -> [Hazard] -> Spaceship
--que simula el resultado de maniobrar una nave a través de una serie de peligros. Si se encuentra un objeto
--pequeño y la nave está escudada no se produce impacto. Si el objeto es grande y la nave está armada entonces
--se transforma en un objeto pequeño. Si es un torpedo no se puede evitar el impacto.

--test :: [Spaceship] -> [Hazard] -> [Spaceship]
--que dada una lista de naves y una lista de peligros retorna la lista de naves que sobreviven los peligros (es decir
--las naves con motores funcionales luego de navegar a través de los meteoros).