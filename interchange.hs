--http://blog.sigfpe.com/2008/05/interchange-law.html
-- (see if it can be improved based on this http://www.stephendiehl.com/posts/adjunctions.html)
{-# LANGUAGE RankNTypes #-}

import Test.QuickCheck
import Control.Monad.Writer

type Natural f g = forall a. f a -> g a  -- functor to functor mapping

-- interchange law
-- two sides of this identity
--  (nat2' . nat1') horizcomp (nat2 . nat1) = (nat2' horizcomp nat2) vertcomp (nat1' horizcomp nat1)

{-

f, f', g, g' are functors.
t: f -> g and s: f' -> g' are  natural transformations.

    -----  f ----->    ------ f' ----->                   -----  f' f ------->
   /      ||       \ /        ||       \                /         ||           \
  H       || t      H         || s      H   horizcomp  H          ||  s o t     H
  \       ||       / \        ||        /              \          ||           /
   \      \/      /   \       \/       /                \         \/          /
    ----  g  ----->    ------ g' ----->                   -----  g' g -------> 

H = Hask

Definition of horizontal composition:

Let x be f' (f c):
Then in 'fmap t x', fmap will be resolve to fmap of f'
So t will be lifted to: f' (f c) -> f' (g c)
So, since s : f' -> g', applying argument (fmap t x) to s returns g' (g c).
Hence one way to define implementation of horizcomp is : s (fmap t x) 

Let x be f' (f c):
Then 's x' resolves to  g' (f c).
Then fmap t is of type ::  g' f c -> g' g c due to fmap resolving to fmap of g'
Then fmap t (s x) = g' (g c)
Hence another way to define the implementation of horizcomp is  : fmap t (s x)

-}

o,o' :: (Functor f, Functor f', Functor g, Functor g') => Natural f' g' -> Natural f g -> (forall c. f' (f c) -> g' (g c))

o s t x = s (fmap t x)
o' s t x = fmap t (s x)

{-
Here is a quick proof of interchange law:

 
    -----  F ----->    ------  F' ----->
          ||                   ||
          || a                 ||  a'
          ||                   ||
          \/                   \/ 
    ----  G  ----->    ------  G' ----->
          ||                   ||
          || b                 ||  b'
          ||                   ||
          \/                   \/ 
    ----  H  ----->    ------  H' ----->

Create a diagram show-casing the naturality of a, a', b and b' (natural transformations):

     --------------- F' (b . a) ----------------->       
                                                        |
    F'F----- F' a -----> F'G  ------ F' b ----->F'H     |
     | \                  |                      |      |
     |  \                 |                      |      |
    a'-F \__ a' o a __   a'-G                   a'-H    |
     |                \   |                      |      |
     |                 \  |                      |      |
     v                  \ v                      v      |
    G'F ---- G' a ---->  G'G  ------ G' b ----->G'H   (b' . a')-H
     |                    | \                    |      |
     |                    |  \                   |      |
    b'-F                b'-G  \___ b' o b ___   b'-H    | 
     |                    |                   \  |      |
     v                    v                    \ v      |
    H'F ---- H' a ---->  H'G  ------ H' b ----->H'H

Notice that F' b . F' a = F' (b . a) = F' o (b . a).
This is due to whiskering. Note that the last F' is not a functor but its Identity natural transformation).

Also, notice that b'-H . a'-H = (b' . a')-H

All squares are commutative (due to naturality on a, a', b, b').
=> Outer square is commutative

Diagonally going source of arrow (b' o b) = target of arrow (a' o a) hence normal composition = (b' o b) . (a' o a)
Above diagonal result is equal to going from either side  =  (b' . a')-H . F' (b . a) = (b' . a') o (b . a)

(b' o b) . (a' o a) =  (b' . a') o (b . a)

-}
-- Now some functors to play with. we need 4: Pair, Id, Maybe, []

{-
      
    ----- (,) ----->    ----- Pair ----->
          ||                   ||
          || alpha'            ||  alpha
          ||                   ||
          \/                   \/ 
 H  ------Id ------>  H -----  [] ------>  H
          ||                   ||
          || beta'             ||  beta
          ||                   ||
          \/                   \/ 
    ---- Either --->    ---- Maybe ------>


H = Hask
(beta . alpha) o (beta' . alpha') = (beta o beta') . (alpha o alpha')

 -}

data Pair x = Pair x x deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

newtype Id x = Id x deriving (Eq, Show)

instance Functor Id where
  fmap f (Id x) = Id (f x)

alpha :: Natural Pair []
alpha (Pair x y) = [x, y]

beta :: Natural [] Maybe
beta [] = Nothing
beta (x:xs) = Just x

alpha' :: Natural ((,) a) Id
alpha' (a,x) = Id x

beta' :: Natural Id (Either b)
beta' (Id x) = Right x

lhso = (beta . alpha) `o` (beta' . alpha')
rhso = (beta `o` beta') . (alpha `o` alpha')

lhso' = (beta . alpha) `o'` (beta' . alpha')
rhso' = (beta `o'` beta') . (alpha `o'` alpha')

type From = Pair (Float, Integer)        -- a composite functor, Pair of comma
type To = Maybe (Either String Integer)  -- a composite functor Maybe of Either

instance Arbitrary a => Arbitrary (Id a) where
  arbitrary = liftM Id arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary

test1 = quickCheck (\x -> lhso (x :: From) == (rhso x :: To))
test2 = quickCheck (\x -> lhso' (x :: From) == (rhso' x :: To))

main = do
  test1
  test2
