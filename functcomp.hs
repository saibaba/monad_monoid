-- https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem
-- https://stackoverflow.com/questions/19774564/what-does-it-mean-to-compose-two-functors
-- https://bartoszmilewski.com/2015/01/20/functors/

-- identity functor - just a holder of a value
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Eq m) => Eq (Identity m) where
  (==) a b = (runIdentity a) == (runIdentity b)

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
  -- fmap f = compose . fmap (fmap f) . decompose
  -- equivalently, signifying the fact that fmap of composite is composite of fmaps of components; this form also helps with seeing association of composition.
  fmap f = compose . ( ( fmap . fmap ) f )  . decompose

test3:: Compose [] Maybe Integer
test3 = fmap (+1) $ Compose [Just 10, Nothing, Just 20]

test4, test5::Maybe (Maybe Integer)
test4 = Just Nothing -- !!
test5 = Just (Just 20) -- !!

-- strech further, 5 levels
test6 = fmap (+2) $ Compose [(Compose [(Compose (Just (Compose [Just 10, Nothing, Just 20])))])]

-- functors with above composition form a category:
-- a) objects = categories (for now ignore fact that category of categories is a category that could could result in paradoxes)
-- b) functors are morphisms (as they map from category to category)
-- c) There is a trivial identity functor
-- d) functor composition is associative

-- consider proving them

-- identity
data_id = Just (20::Integer)

left_identity_lhs = (id . fmap)
left_identity_rhs = (fmap . id)

test_left_identity = left_identity_rhs even data_id == left_identity_lhs even data_id 

-- ignoring Compose and directly using fmap
assoc_lhs f =  ( (fmap .  fmap) . fmap ) f
assoc_rhs f = ( fmap .  (fmap . fmap) ) f

d = [ [Just (2::Integer), Nothing, Just 4] , [Just 1, Just 3, Nothing] ]

test_assoc = assoc_lhs even d  == assoc_rhs even d

main = do
  --print test1
  print (showmb test2)
  -- print test3
  print test4
  print test5
  --print (showmb $ ListOfMaybe [Just 10, Nothing, Just 20])
  --print test6
  print test_assoc
  print test_left_identity
