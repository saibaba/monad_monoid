import Prelude hiding(fmap, join)

unit x  = [x]
join:: [[a]] -> [a]
join = concat
fmap = map
bimap mu fn = mu . fmap fn
mv <=> f = bimap join f mv

test = [0..4] <=> (\v1 ->
  [0..(v1-1)] <=> (\v2 -> 
  [0..(v2-1)] <=> (\v3 -> unit (v1 * v2 * v3))))

main = do
  print test

{--
0 empty
1, 0, empty
2, 0, empty
2, 1, 0 => 0
3, 1, 0 => 0
3, 2, 0 => 0
3, 2, 1 => 6
4, 1, 0 => 0
4, 2, 0 => 0
4, 2, 1 => 8
4, 3, 0 => 0
4, 3, 1 => 12
4, 3, 2 => 24
--}
