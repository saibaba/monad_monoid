import Prelude hiding (id, map)

map :: (Integer -> Integer) -> ([Integer] -> [Integer])
map f a = fmap f a

id x = x

main = do
  -- map id = id (or map (\x -> x) M === M)
  putStrLn (show ( map id [1, 2, 3]))
  -- map f . unit = unit . f
  putStrLn (show ( map (+ 1) [1, 2, 3]))
  -- map (g . f ) = (map g . map f) (or map ( (\x -> f x) . (\y -> g y) ) 
  putStrLn (show ( map ( (+ 1) . (* 3) )  [1, 2, 3]))
  putStrLn (show ( ( ( map (+ 1) ) . ( map (* 3)) ) [1, 2, 3]))
