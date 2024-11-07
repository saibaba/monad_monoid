{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding(Functor, fmap, Monad, id)
import Data.Constraint

-- Morphisms
type (a ~> b) c = c a b

class Category (c :: k -> k -> *) where
  id :: (a ~> a) c
  (.) :: (y ~> z) c -> (x ~> y) c -> (x ~> z) c

type Hask = (->)
 
instance Category Hask where
  id x = x
  (f . g) x = f (g x)

class (Category c, Category d) => Functor c d t where
  fmap :: c a b -> d (t a) (t b)

newtype Id a = Id a

instance Functor Hask Hask Id where
  fmap f (Id a) = Id (f a)

instance Functor Hask Hask [] where
  fmap f [] = []
  fmap f (x:xs) = f x : (fmap f xs)

instance Functor Hask Hask Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)


type Endofunctor c t = Functor c c t

newtype FComp g f x = C { unC :: g (f x) }
newtype Hom (c :: * -> Constraint) a b = Hom (a -> b)

instance (Functor a b f, Functor b c g, c ~ Hom k) => Functor a c (FComp g f) where
  fmap f = (Hom C) (Prelude..) (fmapg (fmapf fw) (Prelude..) (Hom unC))
    where
      fmapf = fmap :: a x y -> b (f x) (f y)
      fmapg = fmap :: b s t -> c (g s) (g t)

type Nat c f g = forall a. c (f a) (g a)


type NatHask f g = forall a. (f a) -> (g a)

-- Functor category
newtype Fun f g a b = FNat (f a -> g b)

-- Endofunctor category
type End f = Fun f f

instance Category (End f) where
  id = FNat id
  (FNat f) . (FNat g) = FNat (f (Prelude..) g)

class Endofunctor c t => Monad c t where
  eta :: c a (t a)
  mu  :: c (t (t a)) (t a)


(>>=) :: (Monad c t) => c a (t b) -> c (t a) (t b)
(>>=) f = mu (Prelude..) fmap f
 
return :: (Monad c t) => c a (t a)
return = eta


test1 = "Hello"

main = do
  print test1

