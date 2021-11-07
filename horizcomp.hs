import Control.Monad(join)

t3 = [ [ [ 1, 2, 3] ] ]

--alpha = join
--beta  = id

{-
F = T2
F' = T
αa :: F a -> F'a
G = T
G' = T
β = id

G (F a) to G'(F'a)

T3 -> T2 a

G'αa ∘ βF a

fmap join . id


βF'a ∘ G αa

id . fmap join

-}


-- as horizontal composition of n1 and n2 (endofunctor composition)
--hc n1 n2 v = 

-- as bifunctor lifting alpha and beta (two morphisms, which are actually natural transformations as we are in endofunctor category)

bimap alpha beta = beta . fmap alpha

main = do
  putStrLn "Started..."
  print $ join (bimap join id t3)
  print $ join (bimap' join id t3)
  putStrLn "Finsished..."
