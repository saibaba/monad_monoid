{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DatatypeContexts #-}

{-
Exercise: create my own version by replacing <#> by bimap, :<*> Compose etc., 
-}

-- http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html
-- Also see important notes in `Further Reading` : https://wiki.haskell.org/Typeclassopedia#Monoid

import Prelude hiding((<*>), Monoid)

import Control.Monad
import Test.QuickCheck


-- The first goal is to create a pointfree version of unit and associative laws
-- We are using a pair to store 2 instances of monoid (the T^2 of mu:: T^2 -> T), 

lambda :: a -> ((),a)
lambda x = ((),x)

rho :: a -> (a,())
rho x = (x,())

alpha :: ((a,b),c) -> (a,(b,c))
alpha ((x,y),z) = (x,(y,z))

-- This is nothing but bimap for pair (,) bifunctor
(<#>) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
(f <#> g) (x,y) = (f x,g y)


class Monoid m where
  one :: () -> m
  mult :: (m,m) -> m

  law1_left,law1_middle,law1_right :: m -> m
  law1_left   = mult . (one <#> id) . lambda
  law1_middle = id
  law1_right  = mult . (id <#> one) . rho

  law2_left,law2_right :: ((m,m),m) -> m
  law2_left = mult . (mult <#> id)
  law2_right = mult . (id <#> mult) . alpha

instance Monoid Int where
  one _ = 1
  mult (a,b) = a*b

check1 = quickCheck $ \n -> law1_left n == law1_middle (n :: Int)
check2 = quickCheck $ \n -> law1_left n == law1_right (n :: Int)
check3 = quickCheck $ \n -> law2_left n == (law2_right n :: Int)

data Id x = Id x deriving Show
instance Functor Id where
  fmap f (Id x) = Id (f x)

lambda' :: Functor f => f a -> (Id :<*> f) a
lambda' x = Id x

rho' :: Functor f => f a -> (f :<*> Id) a
rho' x = fmap Id x

alpha' :: f (g (h a)) -> f (g (h a))
alpha' = id

-- Paralleling pair (,) above, use endofunctor composition, this is essentially object composition where objects are (endo)functors.
type (f :<*> g) x = f (g x)

(<*>) f g = f. fmap g

-- With following, we are able to turn an endofunctor (composition) into a monoid
class Functor m => Monoid' m where
  one' :: Id a -> m a
  mult' :: (m :<*> m) a -> m a

  law1_left',law1_middle',law1_right' :: m a -> m a
  law1_left'   = mult' . (one' <*> id) . lambda'
  law1_middle' = id
  law1_right'  = mult' . (id <*> one') . rho'

  --It seems below can be written as this as well and there is no significance of it
  --       law2_left',law2_right' :: ( (m :<*> m)  :<*> m) a -> m a
  law2_left',law2_right' :: ( m :<*> (m :<*> m) ) a -> m a
  law2_left'  = mult' . (mult' <*> id)
  law2_right' = mult' . (id <*> mult') . alpha'

instance Monoid' Maybe where
  one' (Id v) = Just v
  mult' Nothing = Nothing
  mult' (Just (Just v)) = Just v
{-
In above, Monoid' definition, can we something like this:
instance (Monoid a, Monad f) => Monoid (f a) where
    mempty = return mempty
    mappend f g = f >>= (\x -> (mappend x) `fmap` g)
-}

check4 = quickCheck $ \n -> law1_left' (Just n) == law1_middle' (Just (n :: Int))
check5 = quickCheck $ \n -> law1_left' (Just n) == law1_right' (Just (n :: Int))
check6 = quickCheck $ \n -> law2_left' (Just (Just (Just n))) == law2_right' (Just (Just (Just (n :: Int))))

-- showing monad is monoid

data Monad m => TranslateMonad m a = TM { unTM :: m a } deriving (Eq,Show)

translate :: Monad m => m a -> TranslateMonad m a
translate x = TM x

instance (Monad m,Functor m) => Functor (TranslateMonad m) where
  fmap f (TM x) = TM (fmap f x)

instance (Functor m,Monad m) => Monoid' (TranslateMonad m) where
  one' (Id x) = TM $ return x

  -- join :: Monad m => m (m a) -> m a -- you are constraining m to be monad so we know we can invoke bind on its instances
  -- Based on the fact that `join mma = mma >>= id`, >>= id below changes `m m a` to `m a`.
  -- Precedence for the following : TM $ (fmap unTM x) >>= id
  mult' (TM x) = TM $ fmap unTM x >>= id

instance Arbitrary a => Arbitrary (Id a) where
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r, promotes a function to a monad
  arbitrary = liftM Id arbitrary

instance (Monad m,Eq (m a),Arbitrary (m a)) => Arbitrary (TranslateMonad m a) where
  arbitrary = liftM TM arbitrary

check7 = quickCheck $ \n -> law1_left' n == law1_middle' (n :: TranslateMonad [] Int)
check8 = quickCheck $ \n -> law1_left' n == law1_right' (n :: TranslateMonad [] Int)
check9 = quickCheck $ \n -> law2_left' n == (law2_right' n :: TranslateMonad [] Int)


main :: IO ()
main  =
 do
   putStrLn "Draft"
   let tm = TM [ TM [1::Int, 2::Int], TM [3::Int] ]
   putStrLn "tm..."
   print tm
   putStrLn "fmap unTM x "
   print $ fmap unTM (unTM tm)
   putStrLn "apply >>= id to it, i.e., mu"
   print $ (fmap unTM (unTM tm)) >>= id
   putStrLn "mult' tm..."
   print $ mult' tm
   putStrLn "End draft "
   putStrLn "Running tests"
   check1
   check2
   check3
   check4
   check5
   check6
   check7
   check8
   check9
