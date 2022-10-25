{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

class BiFunctor f where
  -- Since you are defining bimap in terms of first & second OR first & second in terms of bimap (mutual recursion). You got to supply implementation either for bimap or first & second
  bimap :: (a->c) -> (b->d) -> f a b -> f c d
  first :: (a->c) -> f a b -> f c b 
  first g = bimap g id
  second :: (b->d) -> f a b -> f a d
  second  = bimap id
  bimap g h = first g . second h
  {-# MINIMAL bimap | first, second #-}


instance BiFunctor (,) where
  bimap f g (a, b)  = (f a, g b)

test1 = bimap (+1) (*3) (1, 2)
 
data MyPair a b = MyPair a b

instance BiFunctor MyPair where
  bimap f g (MyPair a b)  = MyPair (f a) (g b)

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

{-

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


-}


newtype CompF c d a = CompF { getCompF :: c (d a) }

compose = CompF
decompose  = getCompF

--instance (Functor g, Functor f) => BiFunctor CompF where
  -- bimap :: (a->c) -> (b->d) -> f a b -> f c d
  -- hc :: (g a -> g' a) -> (f a -> f' a) -> ( (HC g f) a -> (HC g' f') a )
 -- bimap g f  = CompF . f . fmap g . getCompF

instance (Functor c, Functor d) => Functor (CompF c d) where
  fmap f = CompF . ( (fmap . fmap ) f) . getCompF

listOfMaybe = CompF [Just (10::Int), Nothing]
w = fmap (+1) listOfMaybe
test4 = decompose w
--w = bimap id (+1) listOfMaybe


-- to make functor composition a bifunctor, we may have to resort to constraint kinds and scoped type variables as in https://www.stephendiehl.com/posts/monads.html

---- Following is from https://www.reddit.com/r/haskell/comments/5bwuh5/comment/d9s0b48/?utm_source=share&utm_medium=web2x&context=3

-- | the type of natural transformations
type (~>) f g = forall a. f a -> g a

{- Should also try with 
  newtype f :-> g =
    Natural { eta :: forall x. f x -> g x }
-}

-- | the type of (horizontal) composition of type constructors
--newtype HC g f a = HC { unHC :: g (f a) }
data HC g f a = HC { unHC :: g (f a) } deriving (Show)

--instance Show (HC g f a) where
--  show x = "whatever"

-- | horizontal composition of natural transformations
hc :: Functor g => (g ~> g') -> (f ~> f') -> ((g `HC` f) ~> (g' `HC` f'))
g `hc` f = HC . g . fmap f . unHC

hc2 :: Functor g' => (g ~> g') -> (f ~> f') -> ((g `HC` f) ~> (g' `HC` f'))
g `hc2` f = HC . fmap f . g . unHC

{-
A contrived example of horizontal composition - in general it is used in framework like monad, but not as a concrete example.
This example is to provide a concrete example to better understand horizontal composition.

WEH = WithErrorHandling, convert Maybe to Either String to add error message for nothing case.
ES = Either String

So, the horizontal composition takes the composite functor List[Maybe a] to another composite functor IndexedList [(Int, Either String a)]. And this is valid for all a.


          Maybe            List
         ------          --------
           |                |          
HASK      WEH     HASK   indexList    HASK
           |                |
           v                v
         -----           -------
          ES            IndexedList

horizontal composition = indexList o weh

Convert each item in a list of Maybes, into Either String, and index list (i.e., add index position with each element).

Requirement: WEH and IndexedList have to be generic functions, i.e., be able to work forall a, i.e., natural transformations.

-}

listOfMaybe2 = HC [Just (10::Int), Nothing, Just (11::Int)]

data IndexedList a = IndexedList [(Int, a)] deriving (Show, Functor)

indexList :: forall a. [a] -> IndexedList a
indexList x = IndexedList (zip [0..] x)

withErrorHandling :: forall a. Maybe a -> Either String a
withErrorHandling Nothing = Left "Nothing?"
withErrorHandling (Just x) = Right x

test5a = (indexList `hc` withErrorHandling) listOfMaybe2
test5b = (indexList `hc2` withErrorHandling) listOfMaybe2

class NatBiFunctor b where
  bimap2 :: Functor g => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f')

instance NatBiFunctor HC where
    bimap2 f g = HC . g . fmap f . unHC

test6 = bimap2 withErrorHandling indexList listOfMaybe2

main = do
  print test1
  print test2a
  print test2b
  print test2
  print test3
  print test4
  print test5a
  print test5b
  print test6
