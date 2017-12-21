import Control.Monad(join)

{-
 - \x->getLine :: a -> IO String
 - fmap print :: IO String -> IO (IO ()) (fmap lifts print from String -> IO () to IO String -> IO(IO())
 - join :: IO (IO ()) -> IO () (removes 1 level of nesting, flattens)
 -
 - How did we come up with it?
 - a -> IO String is Kleisli arrow
 -
 - Given a monad (T, eta, mu) or (T, return, join)
 -
 - Composition of two kleisli arrows:
 - f : a -> T b
 - g : b -> T c
 -
 - then,
 - mu . T g . f  
 - or
 - join . fmap g . f
 -
 - See wikipedia definition of kleisli arrows.
 - Also see: https://www.quora.com/In-Haskell-programming-language-what-are-some-practical-uses-of-Kleisli-composition
 -}
x=join . (fmap print) . (\x -> getLine)

main = do
  print "Type something "
  x 20
