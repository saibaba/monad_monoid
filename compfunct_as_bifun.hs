-- composite functor as bifunctor. 

class BiFunctor f where
  bimap :: (a->c) -> (b->d) -> f a b -> f c d
  first :: (a->c) -> f a b -> f c b 
  first g = bimap g id
  second :: (b->d) -> f a b -> f a d
  second  = bimap id
  bimap g h = first g . second h

instance BiFunctor (,) where
  bimap f g (a, b)  = (f a, g b)

test1 = bimap (+1) (*3) (1, 2)
 
data MyPair a b = MyPair a b

instance BiFunctor MyPair where
  bimap f g (MyPair a b)  = MyPair (f a) (g b)
  first g = bimap g id
  second = bimap id

test2a = first odd (1, 2)
test2b = second odd (1, 2)
test2 = bimap odd (+3) (1, 2)


-- Functoriality means that something is a functor. E.g. when someone asks about the functoriality of some construction, they're asking whether or not it can be upgraded to a functor.
-- Functoriality of bifunctor
-- (f′ ◦ f)⊗(g′ ◦ g) = (f′⊗g′)◦ (f⊗g)
-- idX⊗idY = id(X⊗Y)
f = (*2)
f' = (+1)
g = (*4)
g' = (+2)

test3 = (bimap (f' . f) (g' . g) (1, 1)) == ( ( (bimap f' g') . (bimap f g) ) (1, 1))

main = do
  print test1
  print test2a
  print test2b
  print test2
  print test3
