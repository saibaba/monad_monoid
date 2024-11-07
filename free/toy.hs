-- https://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
data Toy b =
    Output b (Toy b)
  | Bell (Toy b)
  | Done

instance Functor Toy where
    fmap f (Output x toy) = Output (f x) (fmap f toy)
    fmap f (Bell     toy) = Bell     (fmap f toy)
    fmap f  Done           = Done

output :: a -> Toy a -> Toy a
output x toy = Output x toy 

bell :: Toy a -> Toy a
bell toy = Bell toy

done :: Toy a
done = Done

subroutine :: Toy Char
subroutine = output 'A' (bell done)

program = subroutine

{-
You have to wire the individual pieces like above in subroutine to create the composite instance of this recursive data structure. 
In the source code, you cannot use declarative like below to create the datastructure itself. 
If you make Toy an instance of monad, these lines start executing on the passed function, but won't generate the syntax tree
like you did in the subroutine.....
So, you want Toy be a free monad

(aha, we are talking about DSLs)

program = do
    subroutine
    bell
    done
-}


showProgram :: (Show a) => Toy a -> String
showProgram (Output a x) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Bell x) =
    "bell\n" ++ showProgram x
showProgram Done =
    "done\n"

main = do
  putStrLn(showProgram program)
