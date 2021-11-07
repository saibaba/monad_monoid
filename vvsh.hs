-- vertical vs. horizontal compositions
--

{-
 
Vertical composition:

  n1: Pair -> []
  n2: []   -> Maybe

n2 . n1

-}

data Pair x = Pair x x deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

n1 :: (Eq a)  => Pair a -> [a]

n1 (Pair a b) =
  if (a == b) then [a]
  else []

n2 :: [a] -> Maybe a
n2 [] = Nothing
n2 (x:xs) = Just x

vcomp n2 n1 = n2 . n1

{-
Horizontal composition:

  a: (,) -> Pair  converts , to Pair
  b: []  -> Maybe takes an array of pair, returns Nothing or Just Pair <sum of first items>  <sum of second items>

  b o a : [ (,) ] -> Maybe Pair

-}

hcomp b a = b . fmap a

--a :: (,) -> Pair
a (x,y) = Pair x y

--b :: [] -> Maybe
b [] = Nothing
b l = Just (Pair 0 0)


main = do
  print $ (vcomp n2 n1) (Pair 3 5)
  print $ (vcomp n2 n1) (Pair 3 3)
  print $ (hcomp b a) []
  print $ (hcomp b a) [(3, 5), (3, 5)]
