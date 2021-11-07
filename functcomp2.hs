{-# LANGUAGE TypeOperators #-}

-- Functor composition has two parts:
-- part 1: compose objects (E.g., MaybeList or ListMaybe and so on..)
-- part 2: compose morphisms (fmap of composed functor unpacks these two levels 
--         (for example a thing stored in List of Maybe's ) and applies f to it, then repackages inside the
--         same two functors.
--

--type (f :<%> g) x = f (g x)


--lm :: ([] :<%> Maybe) Integer
lm = [Just (1::Integer), Nothing, Just (2::Integer)]

-- part 2
--(<%>) :: (Functor f, Functor g) => (f b -> c) -> (a -> b) -> f a -> c
(<%>) f g = f . fmap g
-- note <%> used in two different contexts (a) type :<%> (b) operator <%>

main = do
  print $ (id <%> (fmap (+1))) lm
  print $ (reverse <%> id) lm
  print $ (reverse <%> (fmap (+1))) lm
