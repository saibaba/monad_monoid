{-# LANGUAGE RankNTypes #-}

import Prelude hiding((>>=))
import Control.Monad(join)

{-
In a world where each object keeps a count of its life or something...
Then Functor, Applicative and Monad come along. World existed without then and can still exist, but these guys help make things better, sort of brokering.
But since they themselves are not adding value, so they are more like voluntary service. Should not affect the count of each object during their involvement.
-}

{- Bad implementation -}

-- a is the type of object having count. An Integer is maintained to keep the count for object a.
data Counter a = Counter Integer a deriving (Show)

tm :: forall a b. (a -> b) -> Counter a -> Counter b
-- tm is just a facilitator 
tm f (Counter n a) =  Counter n (f a)

-- Interesting thing to note about bind (>>=) is that a can be anything including another Counter.
-- And in this case, if function passed is id, that enclosed Counter can be simply extracted as it honors the `Counter b` restriction.
-- This is exactly a mu does.
(>>=) :: Counter a -> (a -> Counter b) -> Counter b
infixl >>=

-- This is a bad bind, does not honor associativity
Counter n a >>= f =
  let Counter _ b = f a
  in Counter (n+1) b

{-

Why is above bad?

Convert to fish form for a methodical analsys (see: https://wiki.haskell.org/Monad_laws)

Hint : f fish g = bind g . f

--}

(>=>) :: (a -> Counter b) -> (b -> Counter c) -> (a -> Counter c)
infixl >=>
f >=> g = bind g . f

{--

Reasoning:

f >=> (g >=> h):

f >=> g a =
let
    in m b >>= g

f >=> g a =
let Counter n1 b = f a
    in Counter n1 b >>= g

f >=> g a =
let Counter n1 b = f a
    Counter _ c =  g b
    in Counter(n1+1) c

In summary, fish increments by 1 of the result of call to f. Then it returns that count with the value of call to g as new Counter.

So,

(f >=> g) >=> h a  = Counter(n1 + 2) c
f >=> (g >=> h) a  = Counter(n1 + 1) c

Here the non-associative function involved is fn(x, y) = x+1.
So,
f(f(a, b), c) = f(a+1, c) = a+2
vs
f(a, f(b, c)) = f(a, b+1) = a+1

mu (Counter n2 (Counter n1 a)) = Counter (n1+n2) a
f >=> g = mu . tm g . f

mu mma = bind mma id

-}

(>=>) :: (a -> Counter b) -> (b -> Counter c) -> (a -> Counter c)
infixl >=>
f >=> g = \a -> let
            Counter n v = f a
          in (Counter n v) >>= g

f a = Counter 1 a
g a = Counter 2 a
h a = Counter 3 a

lhs = (f >=> g) >=> h
rhs = f >=> (g >=> h)

{-

It is all about (Tm . mu_d) vs. mu_td. If they are not equal, associativity is lost. 
Hence monad definition includes it as a condition for something being monad.

obj = (M n3 (M n2 (M n1 d)))

mu(x) = M(M x) -> M x
mu (M n2 (M n1 x)) = M (n1+n2) x

mu_d = M n2 (M n1 d) -> M (n1+n2) d

Tm . mu_d obj = M n3+1 (M n1+n2 d)

mu_d . (Tm mu_d) =  M (n1+n2)+n3 a

vs.

mu_td = M n1+n2 (M i a)

mu_d . mu_td = M n3-j-i a

-}

main = do
  putStrLn $ show (lhs "a")
  putStrLn $ show (rhs "a")
