{-# LANGUAGE FlexibleInstances #-}

data Atom = MkAtom Int deriving (Show)

class Rename a where
    findSub :: a -> a

-- A 'normal' instance would look like this
instance Rename Atom where
    findSub (MkAtom x) = MkAtom x

-- Now i want to acchieve something like this
instance Rename ([] Atom) where
    findSub [MkAtom x] = [MkAtom x]

main = do
  putStrLn $ show (findSub (MkAtom 10))
  putStrLn $ show (findSub ([MkAtom 10]))
