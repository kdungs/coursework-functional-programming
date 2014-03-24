module Blatt13 where

import Prelude hiding (min,max)
import Data.Array

-- Vorlage Zustandstransitionsmonade
newtype Trans s a = T { run :: s -> (a,s) }

instance Monad (Trans s) where
  
  return a = T $ \s -> (a,s)

  T trans >>= f = T $ \s -> let (a,s') = trans s in run (f a) s'

-- Aufgabe 13.1
data Stack a = Stack [a] deriving Show

example :: Stack Int 
example = (snd . run m . Stack) [1,2,3,4] where
  m :: Trans (Stack Int) ()
  m = do
    pop 
    x <- pop 
    push 7
    push 9
    pop
    push x

-- Aufgabe 13.2

data Bintree a = Empty 
               | Leaf a 
               | Branch a (Bintree a) (Bintree a) 
               deriving Show


-- Aufgabe 13.3 

catalan :: Int -> Int
catalan 0 = 1
catalan n = sum $ map (\i -> catalan i * catalan (n-1-i)) [0..n-1]

-- Hilfsfunktion von den Folien
mkArray :: Ix i => (i,i) -> (i -> a) -> Array i a
mkArray (min,max) f = array (min,max) $ map (\i -> (i, f i)) $ range (min,max)
