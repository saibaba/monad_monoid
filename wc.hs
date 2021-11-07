--https://stackoverflow.com/questions/7803449/simple-word-count-in-haskell

import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&))

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length) . group . sort . words . map toLower

main = do
  print $ wordCount "A large text with nonsensical words"
