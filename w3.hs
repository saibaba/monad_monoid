{-# LANGUAGE RankNTypes #-}
import Control.Monad(join)
import Test.QuickCheck

import Control.Monad.Identity
import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&))

--bimap :: (Functor f, Functor g, Functor f', Functor g) => forall a . (f a -> f' a) -> (g b -> g' b) -> (f (g a) -> f' (g' a))
bimap mu fn = mu . fmap fn

-- couple of Kleisli arrows

kinc :: Int -> Maybe Int
kinc v = Just (v+1)

kdec :: Int -> Maybe Int
kdec v = Just (v-1)

-- composing them using bifunctor

gn <> fn = (bimap join gn) . fn

lhs =  kdec <> (kinc <> kinc)
rhs =  (kdec <> kinc) <> kinc
chk1 n = lhs n == rhs n
check1 = quickCheck $ \n -> chk1 n

lhs_id n = (bimap id join (Just (Just (Just n))))
rhs_id n = (bimap join id (Just (Just (Just n))))
chk2 :: Int -> Bool
chk2 n = lhs_id n == rhs_id n
check2 = quickCheck $ \n -> chk2 n

--wordCount :: String -> [(String, Int)]
--wordCount = map (head &&& length) . group . sort . words . map toLower

mToLower t = Identity ( map toLower t)
mWords t = Identity (words t)
mSort l = Identity (sort l)
mGroup l = Identity (group l)
mFreq g = Identity (map (head &&& length) g)

-- m a -> (a -> m b) -> m b
mv <=> f = (bimap join f) mv

wordCount mtext = mtext <=> mToLower <=> mWords <=> mSort <=> mGroup <=> mFreq
wordCountExp mtext = mtext <=> mToLower <=> (\lt -> ( (mWords lt) <=> (\w -> ((mSort w) <=> (\sw -> ((mGroup sw) <=> (\g -> (mFreq g))))))))

wordCountExpMl mtext = mtext <=> mToLower <=>
  (\lt -> 
    ( (mWords lt) <=>  
        (\w -> 
          ( (mSort w) <=> 
              (\sw -> 
                ( (mGroup sw) <=> (\g -> 
                    (mFreq g)
                  )
                )
              )
          )
        )
    )
  )

mtext = Identity "Hadoop is the Elephant King  A yellow and elegant thing  He never forgets Useful data or lets An extraneous element cling  Hadoop is an elegant fellow  An elephant gentle and mellow  He never gets mad Or does anything bad Because at his core he is yellow"

main = do
  check1
  check2
  print $ wordCount mtext
  print ""
  print ""
  print $ wordCountExp mtext
