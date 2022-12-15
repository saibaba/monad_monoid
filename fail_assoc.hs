import Control.Monad(join)

data M a = M a Integer deriving (Show)

tm :: (a -> M b) -> M a -> (M (M b))
tm f (M a i) =  M (f a) i

mu (M (M a i) j) = M a (j-i)

(>=>) :: (a -> M b) -> (b -> M c) -> (a -> M c)
infixl >=>
f >=> g = mu . tm g . f

f :: a -> M a
g :: a -> M a
h :: a -> M a

f a = M a 1
g a = M a 2
h a = M a 3

lhs = (f >=> g) >=> h
rhs = f >=> (g >=> h)

{-

It is all about mu_d . (Tm . mu_d) vs. mu_d . mu_td. If they are not equal, associativity is lost. 
Hence monad definition includes it as a condition for something being monad.

mu(x) = M(M x) -> M x
mu (M (M x i) j) = M x (j-i)

Tm . mu_d = M (M a j-i) k

mu_d . (Tm mu_d) =  M a k-j+i

vs.

mu_td = M (M a i) (k-j)

mu_d . mu_td = M a k-j-i

Notice the need to apply mu_d to (Tm mu_d) and to mu_td to get the final objects being compared:
M a k-j+i vs. M a k-j-i

Without this final application of mu_d, we will be comparing only  M (M a j-i) k and M (M a i) (k-j) which are completely different things. The (associative) equality is in terms of commuting square, not a mathematical equal symbol.

-}

obj = M (M (M "Bad" 3) 2) 1

mu_tm_mu_d = mu . (tm  mu)

mu_td = mu . mu

main = do
  putStrLn $ show (lhs "lhs Bad F")
  putStrLn $ show (rhs "rhs Bad F")
  putStrLn $ show "---------------"
  putStrLn $ show (mu_td obj)
  putStrLn $ show (mu_tm_mu_d obj)
  putStrLn $ show "---------------"
  putStrLn $ show "Below are not completely resolved yet to the same corner, but somewhere in the middle of commuting square corners"
  putStrLn $ show ((tm mu) obj)
  putStrLn $ show (mu obj)


{-
Look at this: https://www.sitepoint.com/how-optional-breaks-the-monad-laws-and-why-it-matters/

Most of the things are wrong in that article:
1) usage of null with Optional.ofNullable
2) andThen has nothing to do with map but used in its place when trying to prove associativity is broken

It still brings up good gotchas, but nothing to do with monad laws or breaking them.
-}


{-

(>=>) :: (a -> M b) -> (b -> M c) -> (a -> M c)
f >=> g
  = \a -> let M b fInt = f a
              M c gInt = g b
           in M c (fInt - gInt)
-}

