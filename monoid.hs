-- http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html
-- Also see 6.1 of Category Theory Applied to Functional Programming

{-# LANGUAGE TypeOperators #-}

import Prelude hiding(Monoid, mappend)
import Test.QuickCheck

-- we want to show that "category" of endo-functors is a monoid

type (f :<%> g) x = f (g x)
--(<%>) :: (f b -> c) -> (a -> b) -> f a -> c
(<%>) f g = f . fmap g
-- note <%> used in two different contexts (a) type :<%> (b) operator <%>

-- identity functor
data Id x = Id x deriving Show
instance Functor Id where
  fmap f (Id x) = Id (f x)

-- Take a functor x, compose with Id functor, since its input and outputs are functors, it is a nat. trans.
lambda' :: (Functor f) => f a -> (Id :<%> f) a
lambda' x = Id x
-- Take a functor x, apply map Id over to the value stored in it
-- i.e., rho' Just 10 = Just Id 10
rho' :: Functor f => f a -> (f :<%> Id) a
rho' x = fmap Id x
alpha' :: f (g (h a)) -> f (g (h a))
alpha' = id

class Functor m => Monoid' m where
  one' :: Id a -> m a
  mappend' :: (m :<%> m) a -> m a

  -- law1_lhs' : if Just 10 is passed, lambda' converts to Id Just 10
  -- And Maybe functor has to provide implementation for one', mappend'
  -- (one' <%> id) changes it into Just Just 10
  -- mappend' undoes one level and returns Just 10
  law1_lhs' ::  m a -> m a
  law1_lhs' = mappend' . (one' <%> id) . lambda'
 
 
  -- If Just 10 passed, rho' converts to Just Id 10
  -- (id <%> one') converts into Just Just 10
  -- and mappend' converts into Just 10
  law1_rhs' :: m a -> m a
  law1_rhs' = mappend' . (id <%> one') . rho' 

  --law2_lhs' :: ((m :<%> m) :<%> m) a -> m a
  law2_lhs' :: m (m (m a)) -> m a
  --law2_lhs' = mappend' . (mappend' <%> id)
  law2_lhs' = mappend' . (fmap mappend')    --  m*(m*m)
  law2_rhs' :: m (m (m a)) -> m a
  --law2_rhs' = mappend' . (id <%> mappend') . alpha'
  law2_rhs' = mappend' . mappend'    -- (m*m)*m
  
  -- nat transform, eta or one
  eta_rhs' :: (a-> m b) -> (a -> m (m b))
  eta_rhs' f = fmap f . one' . Id 
  eta_lhs' :: (a-> m b) -> (a -> m (m b))
  eta_lhs' f = one' .  Id . f

  -- nat transform, mu or mappend
  mu_rhs' :: (a-> m b) -> (m (m a)) -> (m (m b))
  mu_rhs' f = (fmap f) . mappend'
  mu_lhs' :: (a-> m b) -> (m (m a)) -> (m (m b))
  mu_lhs' f = mappend' . fmap (fmap f)

instance Monoid' Maybe where
  one' (Id x) = Just x
  mappend' Nothing = Nothing
  mappend' (Just Nothing  ) = Nothing
  mappend' (Just (Just x)) = Just x
 
test_law1' = (law1_lhs' (Just (20::Integer))) == (law1_rhs' (Just (20::Integer)))

test_law2' = 
  (law2_lhs' (Just (Just (Just (20::Integer))))) == (law2_rhs' (Just (Just (Just (20::Integer)))))
  -- given, (<%>) f g = f . fmap g
  -- expansion, lhs = 
  -- mappend' . (mappend' <%> id) JJJ20
  -- mappend' . (mappend' JJJ20)
  -- expansion, rhs = 
  -- mappend' . (id <%> mappend') . alpha' JJJ20
  -- mappend' (id (fmap mappend' JJJ20))
  -- mappend' (id J (mappend' J J20)

test_eta' = (eta_lhs' (\x -> Just (23+x) ) 10) == (eta_rhs' (\x -> Just (23+x)) 10)

test_mu' = (mu_lhs' (\x -> Just (23+x)) (Just (Just 10))) == (mu_rhs' (\x -> Just (23+x)) (Just (Just 10)))

main = do
  print test_law1'
  print test_law2'
  print test_eta'
  print test_mu'
