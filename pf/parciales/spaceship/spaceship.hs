
data Component = Cargo | Engine | Shield | Cannon deriving (Show)

data Spaceship = Module Component Spaceship Spaceship | Plug deriving (Show)

data Direction = Larboard | Starboard
data Size      = Small | Big | Torpedo
type Hazard    = (Direction, Int, Size)

sp1 = Module Shield (Module Cannon Plug Plug)
                    Plug

sp2 = Module Engine (Module Cargo Plug
                                  (Module Cargo Plug
                                               Plug))
                    (Module Cannon (Module Engine (Module Cannon Plug
                                                                 Plug)
                                                  Plug)
                                   (Module Engine Plug
                                                  Plug))


-- Ejercicio 1

-- a)
shielded :: Spaceship -> Bool
shielded Plug = False
shielded (Module c sp1 sp2) = (isShield c) || (shielded sp1) || shielded sp2

isShield :: Component -> Bool
isShield Shield = True
isShield _      = False

-- b)
armed :: Spaceship -> Bool
armed Plug = False
armed (Module c sp1 sp2) = (isArm c) || (armed sp1) || (armed sp2)

isArm :: Component -> Bool
isArm Cannon = True
isArm _      = False

-- c)
thrust :: Spaceship -> Int
thrust Plug = 0
thrust (Module Engine sp1 sp2) = 1 + (thrust sp1) + (thrust sp2)
thrust (Module c sp1 sp2) = (+) (thrust sp1) (thrust sp2)


-- d)
wreck :: Hazard -> Spaceship -> Spaceship
wreck (_, l, z) Plug = Plug
wreck (_, 0, z) _ = Plug
wreck (Larboard, l, z) (Module c sp1 sp2) = if l <= height sp1
                                            then Module c (wreck (Larboard, l-1, z) sp1) sp2
                                            else Module c sp1 (wreck (Larboard, l-1, z) sp2)
wreck (Starboard, l, z) (Module c sp1 sp2) = if l <= height sp2
                                             then Module c sp1 (wreck (Starboard, l-1, z) sp2)
                                             else Module c (wreck (Starboard, l-1, z) sp1) sp2

height :: Spaceship -> Int
height Plug = 0
height (Module c sp1 sp2) = (+) 1 (max (height sp1) (height sp2))

-- Ejercicio 2

foldSS :: (Component -> a -> a -> a) -> a -> Spaceship -> a
foldSS fm bc Plug = bc
foldSS fm bc (Module c sp1 sp2) = fm c (foldSS fm bc sp1) (foldSS fm bc sp2)

-- Ejercicio 3

-- a)
capacity :: Spaceship -> Int
capacity = foldSS (\c sp1 sp2 -> cap c + sp1 + sp2) 0 where
  cap Cargo = 1
  cap _ = 0

-- b)
largest :: [Spaceship] -> Spaceship
largest = foldr1 (\sp1 sp2 -> if capacity sp1 >= capacity sp2
                              then sp1
                              else sp2)

-- dimensions :: Spaceship -> (Int, Int)
-- dimensions s = (height s, width s) where
--   height = foldSS (\_ hsp1 hsp2 -> 1 + max hsp1 hsp2) 0
--   width s = maximum (levelsLength s)
--   levelsLength s = foldSS (\_ llsp1 llsp2 -> 1 : zipWith' (+) llsp1 llsp2)
--   zipWith' f xs ys = let lx = length xs
--                          ly = length ys
--                      in zipWith f xs ys ++ (if lx > ly
--                                             then drop ly xs
--                                             else drop lx ys)

-- d)
manoeuvre :: Spaceship -> [Hazard] -> Spaceship
manoeuvre s hzs = foldr (\hz sp -> manoeuvre' hz sp) s hzs

manoeuvre' :: Hazard -> Spaceship -> Spaceship
manoeuvre' (d, l, Small) s   = if shielded s
                               then s
                               else wreck (d, l, Small) s
manoeuvre' (d, l, Big) s     = if armed s
                               then manoeuvre' (d, l, Small) s
                               else wreck (d, l, Big) s
manoeuvre' (d, l, Torpedo) s = wreck (d, l, Torpedo) s

components :: Spaceship -> [Component]
components Plug = []
components (Module c s1 s2) = components s1 ++ [c] ++ components s2

replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)
