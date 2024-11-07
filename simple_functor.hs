import Prelude hiding(fmap)

import Data.Char (ord)

-- Based on 8.1.1 in http://uu.diva-portal.org/smash/get/diva2:1369286/FULLTEXT01.pdf

-- First a type constructor that acts as our functor, F
-- -- it takes d into F d
-- -- it has fmap to map a function f:a->b to F(f) : F a -> F b

data F d = MkF d deriving (Show)

fmap :: (a->b) -> (F a -> F b)

fmap f (MkF x) = MkF (f x)
  
  
-- a = string, b = char, c = int

f :: String -> Char
f (x:xs) = x

g :: Char -> Int
g c = ord c 

h :: Int -> String

h i = show i




main = do
  let x = "test"
  let r = fmap f (MkF x)
  putStrLn $ show r
  putStrLn $ (h . g. f) x
