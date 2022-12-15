{-
Associative forms for State monad.
-}

import Prelude hiding(return, join)

newtype StateTrans s a = StateTrans { runState :: s -> (a, s) }

fromStoAandS :: Int -> (String, Int)
fromStoAandS c | c `mod` 2 == 0 = ("foo", c+1)
               | otherwise = ("bar", c+1)

-- Kleisli style composition

(>=>) :: (a -> StateTrans s b) -> (b -> StateTrans s c) -> (a -> StateTrans s c)

f >=> g =
  \a -> StateTrans (\s -> let
                            (b, s')  = runState (f a) s
                          in runState (g b) s')

fgh =
  let
    st = StateTrans fromStoAandS
    f = \x -> st
    g = \y -> st
    h = \z -> st
    
    fgh = (\x -> st) >=> (\y -> st) >=> (\z -> st)
    st2 = fgh ""
  in runState st2 0

expand1 = let
  st = StateTrans fromStoAandS
  f = \x -> st
  g = \y -> st
  h = \z -> st
  -- unrolling fgh = ((\x -> st) >=> (\y -> st)) >=> (\z -> st)
  -- and keeping only essentials
  fg_h = \a -> \s -> let
                      (c, s'') = let
                                  (b, s') = runState (f a) s
                                 in runState (g b) s'
                     in runState (h c) s''
  in fg_h "" 0

  {-
     Above is in the form.

     do {
       (c, s'') <- do { (b, s') <- runState (f a) s;
                        runState (g b) s'
            }
       runState (h c) s''
    }
    or abstracting by (c, s'') = y, (b, s') = x, runState (f a) s = x <- m
    runState (g b) s' = f x
    runState (h c) s'' = G y

    do {
       y <- do { x <-m;
                 f x
            }
       G y
    }
     Compare with https://wiki.haskell.org/Monad_laws
  -}

expand2 = let
  st = StateTrans fromStoAandS
  f = \x -> st
  g = \y -> st
  h = \z -> st
  -- unrolling fgh = (\x -> st) >=> ((\y -> st) >=> (\z -> st))
  -- and keeping only essentials
  f_gh = \a -> \s -> let
                      (b, s') = runState (f a) s
                     in let (c, s'') = runState (g b) s'
                        in runState (h c) s''
  in f_gh "" 0

  {-
     Above is in the form.

     do {
       (b, s') <- runState (f a) s;
       do { (c, s'') <- runState (g b) s'';
            runState (h c) s''
       }
    }
    or abstracting by (c, s'') = y, (b, s') = x, runState (f a) s = x <- m
    runState (g b) s' = f x
    runState (h c) s'' = G y

    do {
       x <- m;
       do {
           y <- f x
           G y
       }
    }
    Compare with https://wiki.haskell.org/Monad_laws
  -}

{-
  In the end, both forms are:

  do {
    A: (b, s') <- runState (f a) s
    B: (c, s'') = runState (g b) s'
    C: runState (h c) s''
  }

  You do prepare (A), bake (B) and eat (C) later OR
  You do prepare (A) first, then later  bake (B) and eat (C)
  (need to get and add reference to source)
  They are in that order always and output of each step goes as input to next step.
-}

main = do
  print fgh
  print expand1
  print expand2
