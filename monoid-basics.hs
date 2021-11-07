-- http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html
-- Also see 6.1 of Category Theory Applied to Functional Programming

{-# LANGUAGE TypeOperators #-}

import Prelude hiding(Monoid, mappend)
import Test.QuickCheck

class Monoid m where
  -- doing this way so that it can be used in point-free style later
  one :: () -> m   
  mappend :: (m, m) -> m  

instance Monoid Integer where
  one _ = 1
  mappend (a, b) = a * b


-- law 1  : identity
-- mappend m . one  = mappend one . m

-- need ability to call something like law1_lhs 10
-- but one takes () in input and mult takes (m,m) in input
-- and we want to write in point-free style.
-- so apply some tricks
--

law1_lhs :: (Monoid m) => m -> m
law1_lhs = mappend . (one <#> id) . lambda
law1_middle = id

(f <#> g) (x, y) =  (f x, g y)

-- lambda :: forall a. a -> ((), a)
lambda x = ((), x)


law1_rhs :: (Monoid m) => m -> m
law1_rhs = mappend . (id <#> one) . rho
rho x = (x, ())


-- law 2 : associative

law2_lhs :: (Monoid m) => ((m, m), m) -> m
law2_rhs :: (Monoid m) => ((m, m), m) -> m

law2_lhs = mappend . (mappend <#> id)
law2_rhs = mappend . (id <#> mappend) . alpha

alpha ((x,y), z) =  (x, (y, z))

test_law1 = do
  (law1_lhs 10::Integer) == (law1_rhs 10::Integer)

test_law2 = do
  law2_lhs ( (2::Integer, 3::Integer), 4::Integer) == law2_rhs ( (2::Integer, 3::Integer), 4::Integer) 
  -- expansion, lhs =
  -- mappend . (mappend <#> id) ((2, 3), 4)
  -- mappend . (mappend (2, 3), id 4)
  -- mappend . (mappend (2, 3), 4)  = mappend . (mappend (2, 3), 4) = mappend . (2 * 3, 4) = (2 * 3) * 4
  -- expansion, rhs =
  -- mappend . (id <#> mappend) . alpha ((2, 3), 4)
  -- mappend . (id <#> mappend) . (2, (3, 4))
  -- mappend . (id 2, mappend (3, 4)) = mappend . (2, 3 * 4) = 2 * (3 * 4)

check1 = quickCheck $ \n -> mappend (one (),  n :: Integer) == n
check2 = quickCheck $ \n -> law1_lhs n == law1_middle (n :: Integer)

main = do
  print test_law1
  print test_law2
  check1
  check2
