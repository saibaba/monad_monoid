{-
https://leetcode.com/problems/merge-two-binary-trees/
-}
--import Prelude hiding(concat)


data BinTree a = BiNil | BinTree a (BinTree a) (BinTree a) deriving (Show, Eq)

clone:: BinTree a -> BinTree a
clone BiNil = BiNil
clone (BinTree v t1 t2) = BinTree v (clone t1) (clone t2)

root1:: BinTree Integer
root1 = BinTree 1 (BinTree 3 (BinTree 5 BiNil BiNil) BiNil) (BinTree 2 BiNil BiNil)
root2:: BinTree Integer
root2 = BinTree 2 (BinTree 1 BiNil (BinTree 4 BiNil BiNil)) (BinTree 3 BiNil (BinTree 7 BiNil BiNil))

merge:: (BinTree Integer) -> (BinTree Integer) ->  BinTree Integer
merge BiNil t2 = clone(t2)
merge t1 BiNil = clone(t1)
merge (BinTree v1 l1 r1) (BinTree v2 l2 r2) = BinTree (v1+v2) (merge l1 l2) (merge r1 r2)

main = do
  print $ merge root1 root2

