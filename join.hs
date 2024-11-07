{-# LANGUAGE RankNTypes #-}

import Prelude hiding (id, map)

-- This seems to be useless and error prone code (join is not even associative, prove it).

unit :: Integer -> [Integer]
unit a = [a]

join :: forall t. [ [t] ] -> [t]
join [(x:xs)] = [x]

main = do
  putStrLn (show ( unit 10 ))
  putStrLn (show ( join [ [1, 2, 3]]))
  -- putStrLn (show ( join [ [1, 2, 3], [4, 5, 6] ]))
