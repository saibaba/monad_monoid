import Control.Monad(ap)

-- https://deque.blog/2017/11/13/free-monads-from-basics-up-to-implementing-composable-and-effectful-stream-processing/

--password :: Nat -> (String -> Bool) -> IO Bool
password maxAttempt valid = recur 1 where
  recur n = do
    putStrLn "Password:"     -- Ask the user for a password
    attempt <- getLine       -- Read the password entered by the user
    if valid attempt         -- In case of valid password:
      then do
        putStrLn ("Successful login after " ++ show n ++ " attempt(s).")
        pure True
      else do                -- In case of invalid passord:
        putStrLn ("Login failed: " ++ show n ++ " attempt(s).")
        if n < maxAttempt
          then recur (n + 1) -- * Try again with one less attempt
          else pure False    -- * Stop after reaching maximal number of attempt

data IOSpec a
  = ReadLine (String -> IOSpec a)
  | WriteLine String (IOSpec a)
  | Pure a

instance (Show a) => Show (IOSpec a) where
    show (ReadLine f) = "ReadLine: " ++ "\n" ++ show (f "dummy")
    show (WriteLine x y) = "WriteLine: " ++ x  ++ "\n" ++ show y
    show (Pure a) = "Pure: " ++ show a

readLine :: IOSpec String
readLine = ReadLine (\s -> Pure s)
 
prnLine :: String -> IOSpec ()
prnLine s = WriteLine s (Pure ())

instance Functor IOSpec where
    fmap f (Pure a) = Pure (f a)
    fmap f (ReadLine cont) = ReadLine (\line -> fmap f (cont line))
    fmap f (WriteLine s next) = WriteLine s (fmap f next)

instance Applicative IOSpec where
    pure = Pure
    (<*>) = ap

instance Monad IOSpec where
    (>>=) (Pure a) f = f a
    (>>=) (ReadLine cont) f = ReadLine (\line -> (cont line) >>= f)
    (>>=) (WriteLine s next) f = WriteLine s (next >>= f)

password2 maxAttempt valid = recur 1 where
  recur n = do
    prnLine "Password:"  -- `prnLine` instead of `putStrLn`
    attempt <- readLine  -- `readLine` instead of `getLine`
    if valid attempt
      then do
        prnLine ("Successful login after " ++ show n ++ " attempt(s).")
        pure True
      else do
        prnLine ("Login failed: " ++ show n ++ " attempt(s).")
        if n < maxAttempt
          then recur (n + 1)
          else pure False

interpret :: IOSpec r -> IO r
interpret (Pure a) = pure a
interpret (ReadLine cont) = do
  l <- getLine
  interpret (cont l)
interpret (WriteLine s next) = do
  putStrLn s
  interpret next

xmain = do
  let valid v = length v > 4
  pwd <- password 3 valid
  print pwd
  -- Read a line and return it
  print $ ReadLine (\x -> Pure x)
  -- Print a line "Hello" and return 5
  print $ WriteLine "Hello" (Pure 5)

  -- Read a line and print it twice, and return 5
  print $ ReadLine (\x -> WriteLine x (WriteLine x (Pure 5)))

  -- Chaining them together (printing the line read)
  print $ ReadLine (\x -> WriteLine x (Pure ()))

  -- Read a line and print it with an exclamation mark
  print $ readLine >>= \x -> let y = x ++ "!" in prnLine y
  print $ "------------"
  let pwdinstr = password2 3 valid
  print $ pwdinstr
  ans <- interpret pwdinstr
  print $ ans
