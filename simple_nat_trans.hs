import Prelude hiding(fmap)

import Data.Char (ord)

import Data.Coerce

-- Based on 8.1.1 in http://uu.diva-portal.org/smash/get/diva2:1369286/FULLTEXT01.pdf

-- First a type constructor that acts as our functor, F
-- -- it takes d into F d
-- -- it has fmap to map a function f:a->b to F(f) : F a -> F b

newtype F d = MkF d deriving (Show, Eq)

toF:: d -> F d
toF = coerce

fromF :: F d -> d
fromF = coerce

fmap :: (a->b) -> (F a -> F b)

fmap f (MkF x) = MkF (f x)
 
  
-- a = string, b = char, c = int

f :: String -> Char
f (x:xs) = x

g :: Char -> Int
g c = ord c 

h :: Int -> String

h i = show i


type F d = d


main = do
  let x = "test"
  let r = MkF x
  let result = r == "test"
  putStrLn $ show result
  putStrLn $ (h . g. f) x
