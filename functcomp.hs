-- https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem
-- https://stackoverflow.com/questions/19774564/what-does-it-mean-to-compose-two-functors

-- identity functor - just a holder of a value
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

test1::[String]

test1 = (return . runIdentity) (Identity "abcde")

-- simple composition of two functors resulting in a new functor
newtype ListOfMaybe a = ListOfMaybe [Maybe a]
instance Functor ListOfMaybe where
  fmap f (ListOfMaybe x) = ListOfMaybe $ fmap (fmap f) x

showmb :: (Show a) => (ListOfMaybe a) -> String
showmb (ListOfMaybe x) = show x

test2 = fmap (+1) $ ListOfMaybe [Just 10, Nothing, Just 20]

-- more general case of composition of two functors as a functor
newtype Compose f g a = Compose { getCompose :: f (g a) }
-- could define like 
-- type (f :<*> g) a = f (g a) too with
-- <*> f g = f . fmap g
compose = Compose
decompose  = getCompose

instance (Functor c, Functor d) => Functor (Compose c d) where
  -- fmap fn (Compose fga) = Compose $ fmap (fmap fn) fga
  -- alternatively
  fmap f = compose . fmap (fmap f) . decompose

test3:: Compose [] Maybe Integer
test3 = fmap (+1) $ Compose [Just 10, Nothing, Just 20]

test4, test5::Maybe (Maybe Integer)
test4 = Just Nothing -- !!
test5 = Just (Just 20) -- !!

-- strech further, 5 levels
test6 = fmap (+2) $ Compose [(Compose [(Compose (Just (Compose [Just 10, Nothing, Just 20])))])]

main = do
  --print test1
  print (showmb test2)
  -- print test3
  print test4
  print test5
  --print (showmb $ ListOfMaybe [Just 10, Nothing, Just 20])
  --print test6
