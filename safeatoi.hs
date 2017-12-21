-- http://blog.ssanj.net/posts/2017-06-07-composing-monadic-functions-with-kleisli-arrows.html
import Control.Monad(join)

stringToNonEmptyString :: String -> Maybe String
stringToNonEmptyString s =
  case length s of
              0 -> Nothing
              otherwise -> Just s
 
stringToNumber  :: String -> Maybe Int

stringToNumber s =
  let r = (reads s) :: [(Int,String)]
  in case length r of
          1 -> Just (fst (r!!0))
          otherwise -> Nothing

mjoin :: Maybe (Maybe a) -> Maybe a
mjoin (Just (Just v)) = Just v
mjoin (Just Nothing) = Nothing
 
mfmap g =
  \m -> case m of
             Nothing -> Nothing
             Just a -> Just (g a)    --- line x

compose :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
--compose g f =  join . fmap g . f

-- replace built in maybe & fmap & join with your own to see under the hood
compose g f =  mjoin . mfmap g . f

-- run above through "2017" as input to see that 'line x' above generates Just (Just 2017)

pipeline = stringToNumber `compose` stringToNonEmptyString

main = do
  print $ pipeline "2017"
  print $ pipeline "year 2017"
