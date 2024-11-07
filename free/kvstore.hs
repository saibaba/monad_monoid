import Prelude hiding(Monoid)
import Control.Applicative
import Control.Monad(ap)
import Data.Map (Map)
import qualified Data.Map as Map

data Free f a = Pure a | Roll (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap fn (Pure a) = Pure (fn a)
    fmap fn (Roll x) = Roll (fmap (fmap fn) x)

mu :: Functor f => Free f (Free f a) -> Free f a
mu (Pure x) = x
mu (Roll y) = Roll (fmap mu y) -- or equivalently mu z = fmap (fmap mu) z

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

instance (Functor f) => Applicative (Free f) where
    pure  = Pure
    (<*>) = ap

instance (Functor f) => Monad (Free f) where

    -- Note that `Free f` is monad, not `Free` itself, so got to be careful with >>=: need extra going inside, i.e., map
    -- x >>= f = mu (fmap f x)

    -- (>>=) :: ma -> (a -> m b) -> m b
    (>>=) (Pure a) fn = fn a
    (>>=) (Roll y) fn = Roll (fmap (\b -> b >>= fn) y)

-- Note that KeyValF is not the store itself (not a map) but just a list of Get/Put commands. 
-- It just stores a list of these commands in a tree
-- Someone (an interpreter) has to implement the real store.
data KeyValF next =
    Get String (Maybe String -> next)
  | Put String String next           -- put key and value and then prepend to existing KeyValF `next`

instance Functor KeyValF where
    -- figured by running `ghci -ddump-deriv w.hs` with `data KeyValF ... deriving(Functor)`
    fmap f (Get key fn) =  Get key (\maybeval -> f (fn maybeval))
    fmap f (Put key val next) = Put key val (f next)

liftF :: (Functor f) => f r -> Free f r
-- what does this mean?
liftF command = Roll (fmap Pure command)

put key val = liftF (Put key val ())
get key = liftF (Get key id)

subroutine :: Free KeyValF ()
subroutine = do
    put "x" "3"
    put "y" "5"

-- all these work
--subroutine = (put "x" "3") >>= (\_ -> put "y" "5")
--subroutine = Roll (Put "x" "3" (Pure ())) >>= (\_ -> Roll (Put "y" "3" (Pure ())))
--subroutine = Roll (Put "x" "3" (Roll (Put "y" "3" (Pure ()))))

{-

subroutine = (put "x" "3") >>= (\_ -> put "y" "5")

put a b = Roll (fmap Pure (Put a b  ())) = Roll (Put a b (Pure ()))

subroutine =  Roll (Put "x" "3"  (Pure ()))  >>= (\_ -> Roll (Put "y" "5"  (Pure ()))  )
subroutine =  Roll (fmap (\b -> b >>= (\_ -> Roll (Put "y" "5"  (Pure ()))  ) )  Roll (Put "x" "3"  (Pure ())) )  
subroutine =  Roll (fmap (\b -> b >>= (\_ -> Roll (Put "y" "5"  (Pure ()))  ) )  Roll (Put "x" "3"  (Pure ())) )  

k >> f = k >>= \_ -> f

do r1 <- a
   r2 <- b r1
   r3 <- c r2
   d r3

a >>= (b >>= (c >>= d))


(Roll (Put "x" "3" (Roll (Put "y" "3" (Roll (Get x L(Roll (Get z L(Pure Just "z")))))))))

-}

--program :: Free KeyValF r
program = do
    subroutine
    xv <- get "x"  -- Roll (fmap Pure (Get key id)) = Roll (Get key (\maybeval -> Pure (id maybeval)))
    let throwaway = do { print $ "x = " ++ show xv }
    get "z"

program1 =  Roll (Put "x" "3" (Roll (Put "y" "3" (Roll (Get "x" (\maybeval -> Roll (Get "z" (\maybeval -> Pure (id maybeval))) ))))))

--showProgram :: (Show a, Show r) => Free (Cache a) r -> String
showProgram (Roll (Put key value next)) =
    "put " ++ show key ++ " = " ++ show value ++ "\n" ++ showProgram next
showProgram (Roll (Get key fn)) =
    "get " ++ key ++ "\n" ++ showProgram (fn (Just key))
showProgram (Pure v) = "pure " ++ (show v) ++ "\n"

dump (Roll (Put key value next)) =
    "(Roll (Put " ++ show key ++ " " ++ show value ++ dump next ++ "))"
dump (Roll (Get key fn)) =
    "(Roll (Get " ++ key ++ " L" ++ dump (fn (Just key)) ++ "))"
dump (Pure v) = "(Pure " ++ (show v) ++ ")"

mapStore map (Roll (Put key value next)) =
    "inserted " ++ key ++ " with value " ++ value ++ "\n" ++ ( mapStore (Map.insert key value map) next )
mapStore map (Roll (Get key fn)) =
    "looked up" ++ " for key " ++ key ++ "; val = " ++ show (Map.lookup key map) ++  "\n" ++ ( mapStore map (fn (Map.lookup key map)) )
mapStore map (Pure v) =
    "nothing doing " ++ (show v) ++ "\n"

main = do
    putStrLn "Start"
    putStrLn $ dump program
    let map = Map.fromList [("what", "dunno")]
    let result = mapStore map program
    putStrLn result
    putStrLn "Done"
