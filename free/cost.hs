import Prelude hiding(Monoid)

import Control.Applicative
import Control.Monad(ap)

-- A functor, being a parametrized data type, can store instances of itself (recursive) ... hmm this is nothing but endofunctor composition

-- Let's create an explicit example

-- Either terminal (Pure holding `a`) or store an instance of self (Roll holding functor `f (Free a)`)
-- The concept is more general, but we are restricting Roll to hold functor as that is what we are interested for now.

data Free f a = Leaf a | Roll (f (Free f a))

-- Above is nothing but endofunctor composition, `Free f` is storing an instance of self `Free f a` in  its 'a' argument.
-- The reason we have to use Leaf and Roll is because we need a name for constructor. Nothing is special about them here.

-- First of all we need to make sure `Free f` is a functor - on the surface of it it looks like so due to parametrized by `a`.

-- Like said above, generally f is not a functor, but in the discussion for this program, we are pre-supposing that it is a functor.
instance Functor f => Functor (Free f) where
  fmap fn (Leaf a) = Leaf $ fn a
  -- Here x is `f (Free f a)`, a functor again holding this functor itself. Second fmap lifts fn by `Free f` while first fmap lifts the result by `f`
  fmap fn (Roll x) = Roll (fmap (fmap fn) x)   -- leads to recursive application of fn to the next `Free f`

--this is the same thing as (++) basically
mu :: Functor f => Free f (Free f a) -> Free f a
mu (Leaf x) = x
mu (Roll y) = Roll (fmap mu y) -- or equivalently mu z = fmap (fmap mu) z

-- Let's also make sure `Free f` is a monoid - this is true only f is functor.

data Id x = Id x
instance Functor Id where
  fmap f (Id x) = Id (f x)

class Functor m => Monoid m where
  mempty :: Id a -> m a
  -- Notice how mult takes a bifunctor (endofunctor composition) as argument
  mappend :: m (m a) -> m a

instance (Functor f) => Monoid (Free f) where
  mempty (Id x) = Leaf x
  mappend = mu

-- Now lets also show that `Free f` is monad, on the surface of it, it looks like so, for example let `Free f` be `m`, then:
-- Pure :: a -> m a        -- looks like pure
-- Free :: f (m a) -> m a  -- looks like join

-- Because we are using Monad from standard library, we need Applicative, just ignore it for now.

instance (Functor f) => Applicative (Free f) where
  pure  = Leaf
  (<*>) = ap

instance (Functor f) => Monad (Free f) where
  x >>= f = mu (fmap f x)
  {- If you unroll mu, this is same as
     (Pure x) >>= f = unpure (fmap f (Pure x)) = unpure (Pure (f x)) = f x
     (Roll fx) >>= g = mu (fmap g (Roll fx)) = mu (Roll g(fx)) = Roll (fmap mu g(fx)) = -- Free fx >>= g  = Free (fmap (\b -> b >>= g) fx)  = Free 
  -}

-- Now a sample
data Cache a next =
    Put a next
  | Delete next
  | Done

instance Functor (Cache a) where
    fmap f (Put x next)  = Put x (f next)
    fmap f (Delete next) = Delete (f next)
    fmap f  Done         = Done

liftF :: (Functor f) => f r -> Free f r
liftF command = Roll (fmap Leaf command)

put x  = liftF (Put x ())
delete = liftF (Delete ())
done   = liftF Done

subroutine :: Free (Cache Char) ()
subroutine = put 'A'

program :: Free (Cache Char) r
program = do
    subroutine
    delete
    put 'B'
    done

findCost :: Free (Cache a) r -> Integer
findCost (Roll (Put a x)) = 1 + findCost x
findCost (Roll (Delete x)) = 2 + findCost x
findCost (Roll (Done)) = 0
findCost (Leaf r) = 0

--showProgram :: (Show a, Show r) => Free (Cache a) r -> String
showProgram (Roll (Put a x)) =
    "put " ++ show a ++ "\n" ++ showProgram x
showProgram (Roll (Delete x)) =
    "delete\n" ++ showProgram x
showProgram (Roll (Done)) =
    "done\n"
showProgram (Leaf r) =
    "return " ++ "\n"

main = do
  putStrLn "Start"
  putStrLn $ showProgram program
  putStrLn $ show (findCost program)
  putStrLn "Done"
