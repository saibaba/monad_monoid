{-# LANGUAGE RankNTypes #-}


import Prelude hiding(return)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip stack = let
  ((), newStack1) = push 3 stack
  (a , newStack2) = pop newStack1
  in pop newStack2

-- monad based

mpop :: State Stack Int
mpop = State $ \(x:xs) -> (x, xs)

mpush :: Int -> State Stack ()
mpush a = State $ \xs -> ((), a:xs)

newtype State s a = State { runState:: s -> (a, s) }

return x = State $ \s -> (x, s)
bind (State h) f = State $ \s -> let (a, newState) = h s;
                                     (State g) = f a
                                 in  g newState

stackManip2 = (mpush 3) `bind` (\a -> 
              mpop      `bind` (\a -> 
              mpop      `bind` (\a ->
              return a)))

stackManip2a = (mpush 3) `bind` (\a -> 
                      mpop    `bind` (\a -> 
                      mpop    `bind` (\a ->
                return a)))
stackManip2b = ((mpush 3) `bind` (\a -> 
                mpop      `bind` (\a -> 
                mpop)))      `bind` (\a ->
                return a)

test1 = runState stackManip2 [5, 8, 2, 1]
test1a = runState stackManip2a [5, 8, 2, 1]
test1b = runState stackManip2b [5, 8, 2, 1]

-- plain vanilla composition


f = \a -> \s -> push a s
g = \b -> \s -> pop s
h = \c -> \s -> pop s

compose g f a =
  \s -> let (b, s') = f a s
            (c, s'') = g b s'
        in  (c, s'')

test2 = (compose h (compose g f) ) 3 [5, 8, 2, 1]

non_assoc_manip :: Int -> State Stack Int
non_assoc_manip a = State $ \(x:xs) -> (x, x-a:xs)

init_mv = mpush 5
stackManip3a = ((non_assoc_manip 5)      `bind` (\a -> 
              (non_assoc_manip 4) `bind` (\a -> 
              (non_assoc_manip 3))))  `bind` (\a ->
              return a)

stackManip3b = (non_assoc_manip 5)        `bind` (\a -> 
              (non_assoc_manip 4) `bind` (\a -> 
              (non_assoc_manip 3)  `bind` (\a ->
              return a)))

test3a = runState (init_mv `bind` (\a -> stackManip3a)) []
test3b = runState (init_mv `bind` (\a -> stackManip3b)) []

main = do
  print test1
  print test1a
  print test1b
  print test2
  print test3a
  print test3b

{-
type ST x = Stack -> (x, Stack)

sfmap f xbar = \s -> let (x, s') = xbar s
                     in (f x, s')

unit x = \s -> (x, s)
join xbarbar = \s -> let (xbar, s') = xbarbar s
                         (x, s'') = xbar s'
                      in (x, s'')


bimap mu fn = mu . sfmap fn
-- m a -> (a -> m b) -> m b
mv <=> f = (bimap join f) mv

test = unit (10::Int) <=> 
-}
