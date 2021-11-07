import Prelude hiding (id, map)


unit :: Integer -> [Integer]
unit a = [a]

join :: [ [t] ] -> [t]
join [(x:xs)] = [x]

main = do
  putStrLn (show ( unit 10 ))
  putStrLn (show ( join [ [1, 2, 3]]))
