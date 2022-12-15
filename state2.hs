{-
Experiment for a deeper understanding of state monad.
-}

import Prelude hiding(return, join)

newtype StateTrans s a = StateTrans { runState :: s -> (a, s) }

get = StateTrans $ \s -> (s,s)  

instance Show (StateTrans s a) where
    show _ = show "x"

return a = StateTrans $ \s -> (a, s)

fromStoAandS :: Int -> (String, Int)
fromStoAandS c | c `mod` 2 == 0 = ("foo", c+1)
               | otherwise = ("bar", c+1)
test1 = 
  let 
    (x, c) = fromStoAandS 0
    (y, c') = fromStoAandS c
    (z, c'') = fromStoAandS c'
  in [(x, c), (y, c'), (z, c'')]

test2 = 
  let
    st = StateTrans fromStoAandS
    (x, c) =  runState st 0
    (y, c') =  runState st c
    (z, c'') =  runState st c'
  in [(x, c), (y, c'), (z, c'')]

-- Kleisli style composition

(>=>) :: (a -> StateTrans s b) -> (b -> StateTrans s c) -> (a -> StateTrans s c)

f >=> g =
  \a -> StateTrans (\s -> let
                            (b, s')  = runState (f a) s
                          in runState (g b) s')

test3 =
  let
    st = StateTrans fromStoAandS
    fgh = (\x -> st) >=> (\y -> st) >=> (\z -> st)
    {-
      or  f=g=h=\xyz -> st
      fgh = f >=> g >=> h
    -}
    st2 = fgh ""
  in runState st2 0

test4 =
  let
    st = StateTrans fromStoAandS
    f = \x -> st
    g = \y -> st
    h = \z -> st
    -- ugly, if you want to be able to access \x, \y, \z etc.,
    -- well, yea, you are using StateTrans to store state but then again creating new states of actions
    -- without leveraging StateTrans. You are essientially creating your own fish operator.
    -- You should consider using a different StateTrans function
    -- that keeps not only state but also actions in the state variable 's' passed around.
    fgh = (\x -> StateTrans (\s ->
                              let 
                                (y, s') = runState (f x) s
                                (z, s'') = runState ((g >=> h) y) s'
                              in ([(x, s'), (z, s'')], s'')))
    st2 = fgh ""
  in runState st2 0

bind:: (StateTrans s a) -> (a -> StateTrans s b) -> StateTrans s b

bind st f = StateTrans (\s ->
  let
    (a, s') = runState st s
  in runState (f a) s')

-- eventhough fish and bind are equivalent, see how it is easy with bind to nest due to its signature (i.e., taking monad and a function as argument instead of two functions). The ugliness in test4 does not show up anymore.

test5 = 
  let
    st = StateTrans fromStoAandS
    st2 = st `bind` (\x ->
                    (st `bind` (\y -> st)))
  in runState st2 0

-- h stands for full history
fromStoAandS2 :: (Int, [(String, Int)]) -> (String, (Int, [(String, Int)]))
fromStoAandS2 (c,h) | c `mod` 2 == 0 = ("foo", (c+1, ("foo", c):h))
                    | otherwise      = ("bar", (c+1, ("bar", c):h))

test6 =
  let
    st = StateTrans fromStoAandS2
    f = \x -> st
    g = \y -> st
    h = \z -> st
    fgh = f >=> g >=> h
    st2 = fgh ""
  in runState st2 (0, [])

main = do
  putStrLn $ show test1
  putStrLn $ show test2
  putStrLn $ show test3
  putStrLn $ show test4
  putStrLn $ show test5
  putStrLn $ show test6
