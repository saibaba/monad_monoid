{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}   -- https://stackoverflow.com/questions/44359466/create-instance-for-list-of-data-types-haskell


-- Functor composition has two parts:
-- part 1: compose objects (E.g., MaybeList or ListMaybe and so on..)
-- part 2: compose morphisms (fmap of composed functor unpacks these two levels 
--         (for example a thing stored in List of Maybe's ) and applies f to it, then repackages inside the
--         same two functors.
--

data ListOfMaybe a = MkListOfMaybe [Maybe a] deriving (Show)

instance Functor ListOfMaybe where
  -- Notice how there are two functions utlilized: "fmap", and "fmap . f"
  fmap f (MkListOfMaybe l_m_a) = MkListOfMaybe (fmap (fmap f) l_m_a) -- bimap fmap (fmap . f)

lm = MkListOfMaybe [Just (1::Int), Nothing, Just (2::Int)]

test1 = fmap (+1) lm

main = do
  print "Before applying function to composite functor"
  putStrLn $ show lm
  print "After applying function to composite functor"
  putStrLn $ show test1

-- Functor composition can be realized using existing bifunctor, but that is a story for later (here objects are composite functors and morphisms are natural transformation from composite functor to another composite functor - horizontal composition of natural transformation of each component functor)
