-- https://stackoverflow.com/questions/13352205/what-are-free-monads
-- also check out: https://www.tweag.io/blog/2018-02-05-free-monads/

import Prelude hiding(Monoid)


import Control.Applicative
import Control.Monad(ap)


-- A functor, being a parametrized data type, can store instances of itself (recursive)

-- Let's create an explicit example

-- Either terminal (Pure holding a) or store an instance of self (Roll holding functor f (Free a))

data Free f a = Pure a | Roll (f (Free f a))

-- Above is nothing but endofunctor composition, `Free f` is storing an instance of self `Free f a` in  its 'a' argument.
-- The reason we have to use Pure and Roll is because we need a name for constructor. Nothing special about them.

-- First of all we need to make sure `Free f` is a functor - on the surface of it it looks like so due to parametrized by `a`.

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Roll x) = Roll (fmap (fmap f) x)

--this is the same thing as (++) basically
mu :: Functor f => Free f (Free f a) -> Free f a
mu (Pure x) = x
mu (Roll y) = Roll (fmap mu y) -- or equivalently mu z = fmap (fmap mu) z

-- Let's also make sure it is a monoid

data Id x = Id x
instance Functor Id where
  fmap f (Id x) = Id (f x)

class Functor m => Monoid m where
  mempty :: Id a -> m a
  -- Notice how mult takes a bifunctor (endofunctor composition) as argument
  mappend :: m (m a) -> m a

instance (Functor f) => Monoid (Free f) where
  mempty (Id x) = Pure x
  mappend = mu

-- Now lets also show that it is monad, on the surface of it, it looks like so, for example let `Free f` be `m`, then:
-- Pure :: a -> m a        -- looks like pure
-- Free :: f (m a) -> m a  -- looks like join

-- Because we are using Monad from standard library, we need Applicative, just ignore it for now.

instance (Functor f) => Applicative (Free f) where
  pure  = Pure
  (<*>) = ap

instance (Functor f) => Monad (Free f) where
  return = Pure -- or you could use pure which is already defined as Pure via Applicative above. I am using this so if I remove Applicative later, we are good.
  x >>= f = mu (fmap f x)
  {- If you unroll mu, this is same as
     (Pure x) >>= f = unpure (fmap f (Pure x)) = unpure (Pure (f x)) = f x
     (Roll fx) >>= g = mu (fmap g (Roll fx)) = mu (Roll g(fx)) = Roll (fmap mu g(fx)) = -- Free fx >>= g  = Free (fmap (\b -> b >>= g) fx)  = Free 
  -}

-- Now a sample
data Toy a next =
    Output a next
  | Bell next
  | Done

-- We have to make `Toy b` a functor to proceed further
instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

liftF :: (Functor f) => f r -> Free f r
liftF command = Roll (fmap Pure command)

output x = liftF (Output x ())
bell     = liftF (Bell     ())
done     = liftF  Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
    subroutine
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Roll (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Roll (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Roll (Done)) =
    "done\n"
showProgram (Pure r) =
    "return " ++ "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showProgram

main = do
  putStrLn "Start"
  pretty program
  putStrLn "Done"
