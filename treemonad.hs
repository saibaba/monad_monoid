{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-
https://dkalemis.wordpress.com/2014/03/22/trees-as-monads/
-}

import Control.Applicative
import Control.Monad(ap)

data MyTree a = MyLeaf a
              | MyNode (MyTree a) (MyTree a)
              deriving (Show)
-- Note, nobody is saying what `a` can be or cannot be in above tree definition. So, it can be a standalone 'MyTree a' as well. So, you have MyTree (MyTree a). There you go!
 
instance Functor MyTree where
   fmap f (MyLeaf x) = MyLeaf (f x)
   fmap f (MyNode x y) = MyNode (fmap f x) (fmap f y)

{-- https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative --}
instance Applicative MyTree where
  pure  = return
  (<*>) = ap
 
instance Monad MyTree where
   return = MyLeaf
   (MyLeaf x) >>= f = f x
   (MyNode x y) >>= f = MyNode (x >>= f) (y >>= f)

{-
Using join/mu
-}

join :: Monad m => m (m a) -> m a -- you are constraining m to be monad so we know we can invoke bind on its instances
join mma = mma >>= id

bind  :: (Monad m) => forall a b. m a -> (a -> m b) -> m b
bind m f  = join (fmap f m)

--join_direct :: Monad m => m (m a) -> m a -- direct implementation of join witbout relying on >>=
join_direct :: MyTree (MyTree a) -> MyTree a
join_direct (MyLeaf a) = a
join_direct (MyNode l r) = MyNode (join_direct l) (join_direct r)
bind_direct m f  = join_direct (fmap f m)

-- How about mappend, what 2 things are getting combined in the underlying monoid?

-- And the composition UF for an adjoint pair is a monoid object in the category of endofunctors on the category.


-- https://stackoverflow.com/questions/15054765/how-do-you-implement-monoid-interface-for-this-tree-in-haskell
--instance (Monoid a, Monad f) => Monoid (f a) where
--    mempty = return mempty
--    mappend f g = f >>= (\x -> (mappend x) `fmap` g)
--append f g = f >>= (\x -> (append x) `fmap` g)


main :: IO ()
main  =
   do
      putStrLn "Program begins."
 
      let tree1 = MyNode (MyLeaf 3) (MyNode (MyLeaf 4) (MyLeaf 5))
      print tree1
 
      putStrLn "Tests that prove that MyTree behaves as a Monad."
 
      print (tree1 >>= (\x -> MyNode (MyLeaf x) (MyLeaf (x+200))))
 
      putStrLn "--- With bind (first one is just fmap, next one is mu on it ---"
      print (fmap (\x -> MyNode (MyLeaf x) (MyLeaf (x+200))) tree1)
      print (tree1 `bind` (\x -> MyNode (MyLeaf x) (MyLeaf (x+200))))

      putStrLn "--- With direct implementation of join/mu without depending on >>= ---"
      print (join_direct (fmap (\x -> MyNode (MyLeaf x) (MyLeaf (x+200))) tree1))
      print (tree1 `bind_direct` (\x -> MyNode (MyLeaf x) (MyLeaf (x+200))))

      putStrLn "--- As Monoid ---"
      let tree_l1_1 = MyNode (MyLeaf 3) (MyLeaf 4)
      let tree_l1_2 = MyNode (MyLeaf 5) (MyLeaf 6)
      let tree_l1_3 = MyNode (MyLeaf 7) (MyLeaf 8)
      let tree_l1_4 = MyNode (MyLeaf 9) (MyLeaf 10)
      let tree_l2_1 = MyNode (MyLeaf tree_l1_1) (MyLeaf tree_l1_2)
      
      let tree_l3 = MyLeaf tree_l2_1
      print (tree_l3)
      -- if associative, following two will be identical (mu . mu = mu . fmap mu, on the lhs mu instantiated at `MyTree a` while on right it is instantiated at `a`)
      print (join (join tree_l3))
      print (join (fmap join tree_l3))
      putStrLn "Program ends."

{-

Input: tree1 = MyNode (MyLeaf 3) (MyNode (MyLeaf 4) (MyLeaf 5))
via >>=: MyNode (MyNode (MyLeaf 3) (MyLeaf 203)) (MyNode (MyNode (MyLeaf 4) (MyLeaf 204)) (MyNode (MyLeaf 5) (MyLeaf 205)))

Before mu, just fmap: MyNode (MyLeaf (MyNode (MyLeaf 3) (MyLeaf 203))) (MyNode (MyLeaf (MyNode (MyLeaf 4) (MyLeaf 204))) (MyLeaf (MyNode (MyLeaf 5) (MyLeaf 205))))
Final, after mu: MyNode (MyNode (MyLeaf 3) (MyLeaf 203)) (MyNode (MyNode (MyLeaf 4) (MyLeaf 204)) (MyNode (MyLeaf 5) (MyLeaf 205)))

-}
