-- a great explanation in http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
--
-- Following two are for understanding deeper details
-- https://stackoverflow.com/questions/35349159/haskell-join-on-state-monad
--https://www.packtpub.com/mapt/book/application_development/9781786461353/5/ch05lvl1sec43/writing-a-state-monad

-- Q: What is a state: A state is something like the current values of mutable fields of an object in OOPL
-- Q: What is a state function: a function that takes current state, and returns an intermediate value and a new state
-- Q: How ? \state -> (intermediate_value, new_state)
-- Q: Example, please? Consider current state, a counter, c. If the counter is divisible by 5, create a value "foo" else create a value "bar" and return  c+1 as new state in either case.
-- Q: What the heck is this "intermediate value". It is probably not used half the time. But, if there is a case where generating next state depends on this intermediate value, it could be used.
-- Q: Are you sure about it? May be not. Because, it can be used some general purpose functions like 'get' to retrieve the current state as intermediate value without affecting state while chaining. Sometimes, some state functions update the state without needing to look at current state and in that case it is not needed/used.

fromStoAandS :: Int -> (String, Int)
fromStoAandS state | state `mod` 5 == 0 = ("foo", state+1)
                   | otherwise  = ("bar", state+1)
-- Q: What is State Monad? just place holder for state function to enable monadic operations (creating new, combining state function)
--

newtype State s a = State { runState :: s -> (a,s) }

stateIntString :: State Int String
stateIntString = State fromStoAandS

-- Monad, being a functor, can be indexed by only one type constructor argument, so it is not State but "State s" or "State Int" in this case is the monad. So, m in "m a" corresponds to "State s"

-- runState together wrapping function instead of state helps to postpone entire execution of state transitions - you want it for example, if you want to call the "composite set of transitions" recursively for smaller values.

mreturn a = State $ \s -> (a, s) -- this is essentially (,) :: a -> b -> (a, b)

-- Q: How do we support composing two state updates? Of course, bind

{--
\s -> (a, s)
\a -> \s -> (b, s)
into
\s -> (b, s) ?

--}

mfmap f (State h) =
  State $ \s -> let (xa, s1) = h s
                in  (f xa, s1)

mjoin :: State s (State s a) -> State s a
mjoin outer = State $ \s ->
  let (inner, s') = runState outer s
  in  runState inner s'

mbind mv mf = mjoin $ mfmap mf mv

-- note that evalState, execState, get, and put are defined as global functions as newtype allows for only one function in its declaration which is used by runState.
--
evalState state s = fst (runState state s)
execState state s = snd (runState state s)

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

type GameValue = Char        -- Unused in this example, a dummy value 'd'!
type GameState = (Bool, Int) -- game is on/off & current score

stepGame s x =
  let (on, score) = s
  in  case x of 'a' | on -> (on, score + 1)
                'b' | on -> (on, score - 1)
                'c'      -> (not on, score)
                _        -> (on, score)

playGame :: String -> State GameState GameValue
playGame []     = mreturn 'd'
playGame (x:xs) =
  get                `mbind` (\a ->    -- here a is actually 'state' as that's what get does, returns state as value
  put (stepGame a x) `mbind` (\_ ->    -- we do not care about return value
  playGame xs))

startState = (False, 0)

main = do
  print $ execState (playGame "abcaaacbbcabbab") startState
  print $ runState (playGame "aaacaaa") (snd (runState (playGame "abcaaacbbcabbab") startState))
