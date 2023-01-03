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

-- Paralleling pair (,) above, use endofunctor composition, this is essentially object composition where objects are (endo)functors. Earlier we used , now we are using :<*> in its place.
type (f :<*> g) x = f (g x)

-- Parallelizing for pair above, <*> is replacing <#>, bimap for composition
(<*>) f g = f. fmap g

{-
An example of <*> usage:

   let w::[Int] = [10, 20]
   print $ ( (take 3) <*> (+1) ) w

Applies g to the content of functor.
Applies f to the resulting functor (same type, but new content) as a whole.

Here input is F a (F = [], a = Int).
f = (+1)
fmap f :: [Int] -> [Int]

g = take 3

g :: [Int] -> [Int]


----

Above bimap can also be considered as a horizontal composition when content are functors.

  let w::[Maybe Int] = [Just 10, Just 20]
  print $ ( (take 3) <*> (fmap (+1)) ) w

Let's say input is  (G (F a))

Then g is a natural transformation : F a -> F' a. In the example above g = fmap (+1),
Then fmap g :: G (F a) -> G (F' a)

Also f is a natural transformation from  G (F' a) -> G' (F' a).

So, f . fmap g :: G F a -> G' F' a. This is horizontal composition of f and g.

Below is an example where f = indexList and g = withErrorHandling
-}

data IndexedList a = IndexedList [(Int, a)] deriving (Show, Functor)
indexList :: forall a. [a] -> IndexedList a
indexList x = IndexedList (zip [0..] x)

withErrorHandling :: forall a. Maybe a -> Either String a
withErrorHandling Nothing = Left "Nothing?"
withErrorHandling (Just x) = Right x


-- With following, we are able to turn an endofunctor (composition) into a monoid
class Functor m => Monoid' m where
  one' :: Id a -> m a
  mult' :: (m :<*> m) a -> m a
  -- Also, notice that both one' and mult' are natural transformations

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
  one' (Id v) = return v
  mult' mma     = mma >>= id

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
-- Notice how we are able to generically create instance of Monoid' for Maybe just using return and >>=. 
-- TranslateMonad given below automates this and demonstrates that any monad can be treated Monoid' generically. It is basically translating a Monad into Monoid'.

data Monad m => TranslateMonad m a = TM { unTM :: m a } deriving (Eq, Show)

translate :: Monad m => m a -> TranslateMonad m a
translate x = TM x

instance (Monad m,Functor m) => Functor (TranslateMonad m) where
  fmap f (TM x) = TM (fmap f x)

instance (Functor m, Monad m) => Monoid' (TranslateMonad m) where
  -- Notice how the return, fmap, >>= of underlying monad, m, are used to define monoid operations
  one' (Id x) = TM $ return x

  -- join :: Monad m => m (m a) -> m a -- you are constraining m to be monad so we know we can invoke bind on its instances
  -- Based on the fact that `join mma = mma >>= id`, >>= id, given below, transforms `m m a` to `m a`.
  -- Precedence for the following : TM $ (fmap unTM x) >>= id
  -- Note that x cannot be any arbitrary monad, but a (T m) monad for mult' compliance :  (m :<*> m) a -> m a
  mult' (TM x) = TM $ fmap unTM x >>= id

instance Arbitrary a => Arbitrary (Id a) where
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r, promotes a function to work on monad
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
   let o::(TranslateMonad [] Int) = one' (Id (10::Int))
   print "One--->"
   print o
   let tm = TM [ TM [1::Int, 2::Int], TM [3::Int] ]
   putStrLn "tm..."
   print tm
   putStrLn "fmap unTM x "
   print $ fmap unTM (unTM tm)
   putStrLn "apply >>= id to it, i.e., mu"
   print $ (fmap unTM (unTM tm)) >>= id
   putStrLn "mult' tm..."
   print $ mult' tm
   -- following two won't work:
   -- print $ mult' (TM (Just "something"))
   -- print $ mult' (TM (TM (Just "something")))
   putStrLn "mult' (TM (Just (TM (Just something))))"
   print $ mult' (TM (Just (TM (Just "something"))))
   let ttm :: TranslateMonad [] (TranslateMonad [] (TranslateMonad [] Int)) = TM [ TM [ TM [1::Int, 2::Int], TM [3::Int] ], TM [ TM [4::Int, 5::Int,6::Int] ] ]
   putStrLn "law2_left' ttm"
   print $ law2_left' ttm

   putStrLn "plain mult'"
   print $ mult' (Just (Just "something"))
   putStrLn "plain law2_left'"
   print $ law2_left' (Just (Just (Just "something")))

   let w::[Maybe Int] = [Just 10, Just 20]
   print $ ( (take 3) <*> (fmap (+3)) ) w

   let listOfMaybe2 = [Just (10::Int), Nothing, Just (11::Int)]
   print $ (indexList <*> withErrorHandling) listOfMaybe2
   putStrLn "End draft"
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
