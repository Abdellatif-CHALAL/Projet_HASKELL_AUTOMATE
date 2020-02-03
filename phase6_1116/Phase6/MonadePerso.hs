module MonadePerso where

data WriterSt a = WriterSt String a deriving (Eq,Ord,Show,Read)

instance Monad WriterSt where
  return x = WriterSt "" x
  (WriterSt s1 x) >>= f = let WriterSt s2 y = f x
                          in WriterSt (s1++s2) y

instance Applicative WriterSt where
  pure x = WriterSt "" x
  (WriterSt s1 f) <*> (WriterSt s2 x) = WriterSt (s1++s2) (f x)

read :: String -> a -> WriterSt a
read s x = WriterSt s x

runWriter :: WriterSt a -> (String,a)
runWriter (WriterSt s x) = (s,x)




data StateP a = StateP (String -> (String,a))
               | Error String

instance Monad StateP where
  return x = StateP (\s -> (s,x))
  (StateP k) >>= f = StateP $ \ s1 ->
                     let (s2,x)   = k s1 in
                     let StateP h = f x in
		     h s2
  (Error st) >>= _ = Error st
  fail st = Error st

-- ceci est un hack pour le projet, ne faites pas
-- ca si vous vous retrouver à coter en Haskell
instance Eq (StateP a) where
  Error s1 == Error s2  =  s1==s2
  x == y                =  False


get :: StateP String
get = StateP (\s -> (s,s))

pop :: StateP Char
pop = StateP (\s -> (tail s,head s))

put :: String -> StateP ()
put c = StateP (\s -> (s,()))

push :: Char -> StateP ()
push c = StateP (\s -> (c:s,()))

state :: (String -> (String,a)) -> StateP a
state = StateP

runState :: StateP a -> String -> (String,a)
runState (StateP k) = k
runSrate (Error st) = error st

isEmpty :: StateP Bool
isEmpty = gets (== "")

runIsEmpty :: StateP a -> String ->  Bool
runIsEmpty (StateP k) s = fst (k s) == ""
runIsEmpty (Error _)  _ = False


modify :: (String -> String) -> StateP ()
modify f = StateP (\s -> (f s,()))

gets :: (String -> a) -> StateP a
gets f = StateP (\s -> (s , f s))



