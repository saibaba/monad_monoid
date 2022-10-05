{-# LANGUAGE TypeOperators #-}

-- Functor composition can be realized using existing bifunctor

-- Functor composition has two parts:
-- part 1: compose objects (E.g., MaybeList or ListMaybe and so on..)
-- part 2: compose morphisms (fmap of composed functor unpacks these two levels 
--         (for example a thing stored in List of Maybe's ) and applies f to it, then repackages inside the
--         same two functors.
--

class BiFunctor f where
  bimap :: (a->c) -> (b->d) -> f a b -> f c d
  first :: (a->c) -> f a b -> f c b 
  first g = bimap g id
  second :: (b->d) -> f a b -> f a d
  second  = bimap id
  bimap g h = first g . second h


newtype ListOfMaybe a = ListOfMaybe [Maybe a]

instance BiFunctor ListOfMaybe where
  bimap f g = f . g

instance Functor ListOfMaybe where
  fmap f = bimap fmap (fmap . f)


lm = [Just (1::Integer), Nothing, Just (2::Integer)]

test1 = (+1) $ ListOfMaybe [Just 10, Nothing, Just 20]

main = do
  print test1
