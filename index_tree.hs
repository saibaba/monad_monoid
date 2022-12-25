{-
https://www.cs.umd.edu/class/spring2019/cmsc388F/lectures/monoids.html
-}

import Prelude hiding((!!))

data Tree v a = Leaf v a
              | Branch v (Tree v a) (Tree v a)  deriving(Show)

type Size = Int

toList :: Tree v a -> [a]

toList (Leaf _ x) = [x]
toList (Branch _ lt rt) = (toList lt) ++ (toList rt)

split myList = splitAt (((length myList) + 1) `div` 2) myList

tag :: Tree Size a -> Size
tag (Leaf t _) = t
tag (Branch t _ _) = t

leaf :: a -> Tree Size a
leaf val = Leaf 1 val
branch :: Tree Size a -> Tree Size a -> Tree Size a
branch l r = Branch (tag l + tag r) l r

fromList :: [a] -> Tree Size a
fromList (x:[]) = leaf x
fromList l      =  let (left, right) = split l
                    in branch (fromList left) (fromList right)

l :: [Size]
l = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

(!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !! 0 = a
(Branch _ x y)  !! n
     | n < tag x     = x !! n
     | otherwise     = y !! (n - tag x)


-- what would a monad be ?
--   tree1, tree2 with individual tags - when combined 

main = do
  print $ (fromList l) !! 6
