{-# LANGUAGE RankNTypes #-}

-- https://stackoverflow.com/questions/34398239/with-monads-can-join-be-defined-in-terms-of-bind

import Prelude hiding(Monad)

class Monad m where
  bind   :: forall a b. m a -> (a -> m b) -> m b
  return ::               a               -> m a

instance Monad Maybe where
  bind (Just x) k = k x
  bind Nothing _ = Nothing
  return a = Just a

{- In bind above
    * important to understand that 'a' can be anything and 'b' can be anything
    * just that the repeated occurences have to match and (a->m b) be compatible with input/outputs
   So, a = m a , b = a can be used as well
  bind :: m (m a) -> ( (m a)  -> m a) -> m a
  We  can use id for (m a) -> (m a).
-}

test1 = bind (Just (Just 20)) id
test2 = bind (Just (Just 20)) (\x -> Just x)

-- test3 = bind (Just 20) id
-- test3, will break. Why?
-- m a -> (a -> m b) -> m b
-- Input to id (which is the 2nd argument to bind) is fine, 'm a'.
-- But output is literal '20', can't be matched with expected Just <Int> (the 'm b' part).
-- For example, if we make the function (2nd argument) return a monad, we are okay again:
test4 = bind (Just 20) (\x -> Just x)

-- So, you can say:
join :: Monad m => m (m a) -> m a -- you are constraining m to be monad so we know we can invoke bind on its instances
join mma = bind mma id

{-

How does m a -> (a -> m b) relate to the natural transformation µ: M . M -> M

https://www.reddit.com/r/math/comments/ap25mr/comment/eg6tkt5/?utm_source=share&utm_medium=web2x&context=3

--}

test5 = join (Just (Just 20))

{- 
Additionally note that
bind m f  = join (fmap f m)
bind m id = join (fmap id m)
          = join (     id m)
          = join (        m)

In categorical terms,

mu = join
bind = mu T f 
-}

main = do
  print test1
  print test2
  print test4
  print test5
