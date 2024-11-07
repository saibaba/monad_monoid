import Control.Monad(join)

f :: String -> Maybe Integer
f s = case s of
  "one" -> Just 1
  "two" -> Just 2
  "three" -> Just 3
  "four" -> Just 4
  "five" -> Just 5
  "six" -> Just 6
  "seven" -> Just 7
  "eight" -> Just 8
  "nine" -> Just 9

g :: Integer -> Maybe Bool

g x = case x `mod`  2 of
  0 -> Just True
  _ -> Nothing


showf :: Maybe Integer -> String
showf (Just x)  = show x
showf Nothing  = "Nothing"

showg :: Maybe Bool -> String
showg (Just x)  = case x of
  True -> "Even"
  False -> "Not Even"
showg Nothing  = "Not Even"

(<=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
infixl <=>
f <=> g = join . fmap g . f

h :: Bool -> Maybe (String, Integer)
h x = case x of
  True -> Just ("Great", 1)
  False -> Just ("Oh no", 0)

showh :: Maybe (String, Integer) -> String
showh (Just (x, y))  =  show (x, y)
showh Nothing  = show ("Oh no", 0)

main = do
  putStrLn $ showh ( ((f <=> g) <=> h) "two" )
  putStrLn $ showh ( (f <=> (g <=> h)) "two" )
