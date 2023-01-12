{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad(join)


{-
F = T2
F' = T
α_a :: F a -> F'a
G = T
G' = T
β = id

G (F a) to G'(F' a)

T3 -> T2 a

G'α_a ∘ β_(F a)

fmap join . id


β_F'a ∘ G α_a

id . fmap join

alpha = join
beta  = id

bimap alpha beta = beta . fmap alpha

-}


-- Natural Transformation ~>
type (~>) f g = forall a. f a -> g a

-- Functor composition (Functor objects)
newtype HC g f a = HC { unHC :: g (f a) } deriving (Show)

-- morphisms are natural transformations like g ~> g'. Horizontal composition of these is the natural transformation from one composite functor to another
hc :: Functor g => (g ~> g') -> (f ~> f') -> ((g `HC` f) ~> (g' `HC` f'))
g `hc` f = HC . g . fmap f . unHC

{-
Functor composition is bifunctor where each component itself is a functor.
The bimap is the horizontal composition.
Since we have to compose natural transformations (i.e., f a -> f' a), not functions (f -> f'), we have to use a different variation of BiFunctor.
-}

class NatBiFunctor b where
  bimap2 :: (Functor f, Functor g) => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f')

instance NatBiFunctor HC where
    bimap2 f g = HC . g . fmap f . unHC

newtype List2d a = MkList2d [[a]] deriving (Functor, Show)
 
compList :: HC [] List2d Int
compList = HC [(MkList2d [[1::Int, 2::Int, 3::Int]])]

concat2d (MkList2d x) = concat x

t1 = bimap2 concat2d id compList

main = do
  putStrLn "Started..."
  print t1
  putStrLn "Finsished..."

-- See bifun.hs for more entertaining example of horizontal composition.

