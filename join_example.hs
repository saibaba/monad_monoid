-- https://evinsellin.medium.com/teaching-monads-slightly-differently-2af62c4af8ce
import Control.Monad(join)

{-
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

data User = MkUser String [String] deriving (Show)

getName (MkUser name tweets) = name
getTweets (MkUser name tweets) = tweets

users = [
  MkUser "Chad Brogrammer" ["lol generics", "hn is my bestie"], 
  MkUser "Freddie Hubbard" ["i make trumpet sounds", "haha good memes there"] ]

allTweets:: [User] -> [String]

allTweets x = join $ fmap (\user -> fmap (\tweet -> "<" ++ (getName user) ++ "> " ++ tweet) (getTweets user)) x

--  Using Kleisli arrow composition
doer f g = join . fmap g . f

userList = id
userTweets user = fmap (\tweet -> "<" ++ (getName user) ++ "> " ++ tweet) (getTweets user)

allTweets2 = doer userList (\user -> userTweets user)

main = do
  print $ allTweets users
  print $ allTweets2 users
