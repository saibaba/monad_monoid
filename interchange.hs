--http://blog.sigfpe.com/2008/05/interchange-law.html

{-# OPTIONS -fglasgow-exts -fno-warn-missing-methods #-}
import Test.QuickCheck
import Control.Monad.Writer

type Natural f g = forall a. f a -> g a  -- functor to functor mapping

-- interchange law
-- two sides of this identity
--  (nat2' . nat1') horizcomp (nat2 . nat1) = (nat2' horizcomp nat2) vertcomp (nat1' horizcomp nat1)

{-
      
    -----  f ----->    ------ f' ----->
          ||                   ||
          || t                 ||  s
          ||                   ||
          \/                   \/ 
    ----  g  ----->    ------ g' ----->

H = Hask

Definition of horizcomp:

Let x be f' (f c):
then in 'fmap t x', fmap will be resolve to fmap of f'
then t will be lifted to: f' (f c) -> f' (g c)
so, if apply result to s, it returns g' (g c).
hence one way  is s (fmap t x) 


Let x be f' (f c):
then s x =>  g' (f c)
then fmap t is of type ::  g' f c -> g' g c due to fmap resolving to fmap of g'
then fmap t (s x) = g' (g c)
hence another way is : fmap t (s x)

  s (fmap t x)

 -}

o,o' :: (Functor f, Functor f', Functor g, Functor g') => Natural f' g' -> Natural f g -> (forall c. f' (f c) -> g' (g c))

o s t x = s (fmap t x)
o' s t x = fmap t (s x)

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
