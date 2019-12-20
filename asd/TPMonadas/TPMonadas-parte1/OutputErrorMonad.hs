module OutputErrorMonad where

import Monadas  

-- COMPLETAR con una definición de tipo 
-- de forma tal que sea al mismo tiempo 
-- instancia de ErrorMonad y de PrintMonad

type Algo = String    
data Pepe a = Pep (a, Algo)
     deriving Show

instance Monad Pepe where
  return x = Pep (x, "")
  m >>= k  = let Pep (v, scr1) = m
              in let Pep (res, scr2) = k v
                  in Pep (res, scr1 ++ scr2)
  fail msg = error msg

instance PrintMonad Pepe where
  printf msg = Pep ((), msg)  

instance ErrorMonad Pepe where
   throw msg = fail msg