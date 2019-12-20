data Celda = C Int deriving Show

data Fila = F Int [Celda] deriving Show

data Dir = Izq | Der deriving Show

type Cmd = Fila -> Maybe Fila

cambiarCelda :: Int -> Celda -> [Celda] -> [Celda]
cambiarCelda 0 cn (c:cs) = cn : cs
cambiarCelda n cn (c:cs) =
  c : cambiarCelda (n-1) cn cs

modifyC :: (Int -> Int) -> Cmd
modifyC f (F n cs) =
  let (C b) = cs !! n
      cn = C (f b)
  in return (F n (cambiarCelda n cn cs))

poner :: Cmd
poner = modifyC (+1)

positiva :: Celda -> Bool
positiva (C n) = n >= 0

-- Prec.: hay bolitas en la celda actual
sacar :: Cmd
sacar f =
  case modifyC (\x -> x - 1) f of
    Just (F n cs) ->
      if all positiva cs
         then return (F n cs)
         else Nothing

mover :: Dir -> Cmd
mover = undefined

ponerN :: Int -> Cmd
ponerN = undefined

sacarN :: Int -> Cmd
sacarN = undefined

moverN :: Dir -> Int -> Cmd
moverN = undefined

muchosPonerYSacar :: Maybe Fila
muchosPonerYSacar =
  do
    let f1 = F 2 [C 1, C 2, C 3]
    f2 <- poner f1
    f3 <- poner f2
    f4 <- sacar f3
    f5 <- sacar f4
    f6 <- sacar f5
    f7 <- sacar f6
    f8 <- sacar f7
    f9 <- sacar f8
    return f9
    -- vamos a ver cómo mejorar esta
    -- secuencia de modificaciones

  -- con ponerN y sacarN quedaria...
  -- do
  --   f2 <- ponerN 2 f1
  --   sacarN 6 f2

main :: IO ()
main = undefined -- hacer test
