module OutputErrorMonad where
import Monadas

type MessageError = String
type Screen = String
type Date = String

data OutputErrorMonad a = O4 Screen a | MyOwnError MessageError deriving Show

instance Monad OutputErrorMonad where
  return x = O4 [] x 
  m >>= k  = case m of
  				MyOwnError msg -> MyOwnError msg 
  				O4 ed x -> case k x of  
  								MyOwnError msg' -> MyOwnError  (msg' ++ "!")
  								O4  ed' x' -> O4 (ed ++ ed') x'  		 
  fail msg = MyOwnError msg

instance PrintMonad OutputErrorMonad where
  printf msg = O4 msg ()   

instance ErrorMonad OutputErrorMonad where
  throw msg = fail msg
