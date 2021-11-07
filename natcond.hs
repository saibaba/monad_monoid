{-

Example for naturality condition:


                        List[Integer]
              ^                              \
             /              |                 \
            /               |                  v
        Integer ----------- |   ---------> Maybe Integer
          |                 |                   |  
          |                 |                   |
          |                                     |
          |              List f                 |
          f                 |                Maybe f
          |                 v                   |
          |           List[String]              |
          |   ^                           \     |
          |  /                             \    |
          v /                               v   v
        String ------------------------>   Maybe String
        

There are many paths from top left (Integer) to bottom right (Maybe String). 
We are focusing on 2 paths specifically (given below):
 
     List[Integer] -> Maybe Integer -> Maybe String   and
     List[Integer] -> List[String]  -> Maybe String
   
    (other paths are covered by Functor laws already) 

Let's say the mapping from List[Integer] -> Maybe Integer is named safeHead

x be an integer.

Say f = show

You would like:

     fmap f (safeHead  List(x))
              =
     safeHead (fmap f (List(x))

This is naturality condition.

Abstracting safeHead = alpha

  fmap f . alpha = alpha . fmap f

-}

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x


f :: Integer -> String

f x = show x




asList x = [x]

l = asList 10

path1 = fmap f (safeHead l)

path2 = safeHead (fmap f l)

main = do
  print $ path1
  print $ path2

