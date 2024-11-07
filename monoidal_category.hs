import Prelude hiding(Monoid, mappend)

-- let's say category has single element, Int
-- Let B be a functor

newtype B = B { getB :: Int } deriving (Eq, Show)

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a


instance Monoid B where
    mempty = B 0
    B x `mappend` B y = B (x + y)

data MonoidalCategory 
class Monoidal f where
  bimap :: (a->b) -> (a->b) -> f a a -> f a a
  first :: (a->c) -> f a b -> f c b 
  first g = bimap g id
  second :: (b->d) -> f a b -> f a d
  second  = bimap id
  bimap g h = first g . second h

instance BiFunctor B => (Monoid B) where
  bimap g h = mappend

main = print $ mappend (B 5) (B 10)
