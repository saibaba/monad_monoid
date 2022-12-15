{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}


import Prelude hiding((>>=), concat)

data Identity a = Identity a deriving (Show, Eq)
runIdentity (Identity x) = x

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data MyList a = MyNil | MyCons a (MyList a) deriving (Show, Eq)

rtn x = MyCons x (MyNil)

concat :: MyList a -> MyList a -> MyList a

concat l1 MyNil = l1
concat MyNil l2 = l2
concat (MyCons a l1) l2  = MyCons a (concat l1 l2)

x::MyList Integer
x = MyCons 3 (MyCons 2 (MyCons 1 (MyNil)))

y::MyList Integer
y = MyCons 20 (MyCons 10 (MyNil))


{-
Associativity of concat ?

Let C stand for list (category).

(C + C) + C = C + (C+C) where + means concat.


We need to prove whether concatinating inner first vs. outer first results in the same object at the end.

                    concat_[a]
[[[a]]] (spot 1) -------------------> [[a]] (spot 2)
  |                                     |
  |                                     |
  | fmap . concat_a                     | concat_a
  |                                     |
  |                                     |
  v                                     v
 [[a]] (spot 3) --------------------->[a] (spot 4)
                    concat_a

(underscore means, instantiated at that object).

Here fmap is that of list, C.
fmap . concat (indexed by a) is C+ (C + C), then by concat (indexed by a) is C.
concat (indexed by [a]) is   (C+C)+C and then by concat (indexed by a) is C.

Above square commutes directly as a result of naturality of concat (Cat Thy Applied to Func Progm, page 58, picture 5.7).
Naturality says that `fmap f . concat = concat . fmap (fmap f)`. Take f = id, so a=b and [a] = [b] in the picture.

Here 'fmap f' = concat_a (concat indexed by a).

concat_a . concat_[a] = concat_a . fmap concat_a

In the diagram `fmap f . concat = concat . fmap (fmap f)`. Take f = id, so a=b and [a] = [b] in the picture.

Also, see Approach 2: program algebra for Question 2 a): https://www.csc.kth.se/utbildning/kth/kurser/2D1456/avfunk07/view.php?arg=H3-a.h

Adding identity to above, using endofunctor category (functors like list are objects, natural transformations like concat are morphism), an element of this category, list can be treated as a moniod. Hence follows associativity law.

What does this all mean for a list?

Consider list xxx2 = [  [[1,2,3]], [[10,20,30],[100,200,300]], [[400]]  ] (spot 1 in the diagram)

concat . fmap concat flattens inner list first, and then flattens the outer list. This is : C+(C+C)
fmap concat xxx2 -> [  [1,2,3], [10,20,30,100,200,300], [400]   ]   (spot 3)
concat . fmap concat xxx2 -> [1,2,3,10,20,30,100,200,300,400]       (spot 4)

concat . concat will flatten outer list first and then the inner list. This is : (C+C)+C
concat xxx2 -> [  [1,2,3], [10,20,30], [100,200,300], [400]  ]   (spot 2)

concat . concat xxx2 ->  [1,2,3,10,20,30,100,200,300,400]        (spot 4)

Note that spot 2 and spot 3 are not identical in terms of the structure. Only that from the type perspective, they are the same.

Pictorially:

                                                                concat_[a]
[  [[1,2,3]], [[10,20,30],[100,200,300]], [[400]]  ] (spot 1) --------------> [  [1,2,3], [10,20,30], [100,200,300], [400]  ]   (spot 2)
  |                                                                                                 |
  |                                                                                                 |
  | fmap . concat_a                                                                                 | concat_a
  |                                                                                                 |  
  |                                                                                                 |
  v                                                                                                 v
 [  [1,2,3], [10,20,30,100,200,300], [400]   ]  (spot 3) ---------------------> [1,2,3,10,20,30,100,200,300,400] (spot 4)
                                                                concat_a

-}

instance Functor MyList where
    fmap f MyNil  = MyNil
    fmap f (MyCons x l) = MyCons (f x) (fmap f l)

--- assoc ---
concatn :: MyList (MyList a) -> MyList a
concatn MyNil = MyNil
concatn (MyCons l rl) = concat l (concatn rl)

{-
With respect to concatn, it seems we are replacing cross product (concat x y) with composition (concatn [x, y]).
So, if we want to replace the cross product with composition of endofunctors, we need an equivalent for natural transformations. 
(ref: https://blog.merovius.de/posts/2018-01-08-monads-are-just-monoids/)
Is this what happening here between concat and concatn?
-}

test_assoc aaa = do
  putStrLn ""
  putStrLn "Testing association of concatenation (concatn . concatn == concatn . fmap concatn)"
  result <- let lhs = (concatn . concatn) aaa
                rhs = (concatn . fmap concatn) aaa
            in  return (lhs == rhs)
  print $ result
  putStrLn "Done"
  putStrLn ""

{-

You might be asking, we are putting two lists in a list in testing the associativity above, but can we put them in a pair and work our way?

See Monoidal categories section: https://bartoszmilewski.com/2016/12/27/monads-categorically/

We have to ask concat to operate on pair instead of List of lists.

concat (x, concat (y, z)) = concat (concat (x, y), z)

(concat . bimap id concat)(x, (y, z))

-}

data MyPair a b = MyPair a b
bimap:: (a -> c) -> (b -> d) -> (MyPair a b) -> MyPair c d
bimap f g (MyPair a b)  = MyPair (f a) (g b)

concatm :: MyPair (MyList a) (MyList a) -> MyList a
concatm (MyPair l1 l2) = concat l1 l2

test_assoc_via_pairs x y z = do
  putStrLn ""
  putStrLn "Testing association of concatenation using pair to hold two lists"
  result <- let p_lhs = MyPair (MyPair x y) z
                p_rhs = MyPair x (MyPair y z)
                -- note how both (concatm . bimap concatm id) and (concatm . bimap id concatm) are in point-free form indicating, 
                -- they are equal at morphism level instead at object level for each object
                lhs = (concatm . bimap concatm id) p_lhs
                rhs = (concatm . bimap id concatm) p_rhs
            in  return (lhs == rhs)
  print $ result
  putStrLn "Done"
  putStrLn ""

{-

Now, about associativity of concatm

Let, concatm (x, y) = [x, y]

concatm (concatm (x, y), z) = [x, y, z] and so is
concatm (x, concatm (y, z)) = [x, y, z]

-}



{-

Do we really need full blown categorical product like MyPair above to establish associativity? 
No, all we need is a bifunctor. We do not need category product and projection operations etc.,
All we need is ability to lift 2 natural transformations (mu and id) to prove the associativity and unit laws (mu . bimap id mu = mu . bimap mu id).

So, given that a functor can contain anything (forall, any object), why not let it contain an instance if itself? The we do not need a new structure
like MyPair. This is endofunctor composition. But you may ask, do not you need to strore two things for bifunctor but not one in itself? That's where single object
monoid comes into play. With monoids and monads, we are always thinking about the "structural" content than payload of the ADT. For example "maybe", "the state", "the count", 
"the pair or comma" than what is actually contained. 
And associativity is dealing with this structural aspect only. So when we are saying one functor contains another as payload, we still have two instances
of the "structure" to deal with - and monoid rules help with associativity of manipulating this structures in any order we want with end result being the same.

Now, the mapping from a (endo)functor  to another is a natual transformation. So, our bifunctor has to lift two unrelated natural transformations. So we need
a more general definition of composition of natural transformations than vertical where output of one needs to match input of another. Horizontal composition nicely fits it.


TODO: Now do the same but using horiz comp

-}


--- end assoc --

z = MyCons 300 (MyCons 200 (MyCons 100 (MyNil)))

test0 = do
  putStrLn "Associativity of concat, i.e. concat (concat x y) z == concat x (concat y z)"
  print $ (concat(concat x y) z) == (concat x (concat y z))
  putStrLn "------"

mu :: MyList (MyList a) -> MyList a
mu MyNil = MyNil
mu (MyCons (MyCons a l1) l2) = MyCons a (concat l1 (mu l2))

(>>=) :: MyList a -> (a -> MyList b) -> MyList b
infixl >>=
l >>= f = mu (fmap f l)

msqr a = rtn (a*a)

test4 = x >>= msqr

-- bind, but this time directly without using mu and fmap
bind:: MyList a -> (a -> MyList b) -> MyList b
infixl `bind`
MyNil `bind` _ = MyNil
(MyCons x l) `bind` f = concat (f x) (l `bind` f)

test5 = x `bind` msqr

-- 3ply nested list
xxx = MyCons (MyCons (MyCons 3 MyNil) (MyCons (MyCons 2 MyNil) (MyCons (MyCons 1 MyNil) MyNil))) MyNil

test 6a = mu xxx

test6 = (mu . fmap mu) xxx

-- Due to associativity (mu. fmap mu = mu . mu) test6 can be written as test7 below
test7 = (mu . mu) xxx

-- Now let's use horiz comp
-- First define nat. trans constraint as map from functor to functor
{-
First we need to solve a problem, we need to make Haskell think that MyList MyList is a functor.
This is so that mu :: MyList (MyList a) -> MyList a is considered a function from a functor MyList MyList to MyList,
hence can be treated as natural transformation instantiated at `a`.

There is no direct way of doing it. We have to wrap inside a Compose (like HC or FComp or Compose etc.,) and compose composes.

https://stackoverflow.com/questions/25210743/bicategories-in-haskell
https://wiki.haskell.org/Type_composition


-}

type (~>) f g = forall a. f a -> g a
data HC g f a = HC { unHC :: g (f a) } deriving (Show)

-- | horizontal composition of natural transformations
hc :: Functor g => (g ~> g') -> (f ~> f') -> ((g `HC` f) ~> (g' `HC` f'))
g `hc` f = HC . g . fmap f . unHC

class NatBiFunctor b where
  -- bimap2 is basically a special case of horizontal composition of two natural transformations (f ~> f') and (g ~> g'). 
  -- It is special case because all catagories involved are just HASK.
  -- See Monoidal categories section: https://bartoszmilewski.com/2016/12/27/monads-categorically/
  bimap2 :: (Functor f, Functor g) => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f')

instance NatBiFunctor HC where
    bimap2 f g = HC . g . fmap f . unHC

instance (Functor g, Functor f) => Functor (HC g f) where
  fmap h (HC gf) = HC (fmap (fmap h) gf)

w f g = g . fmap f

test9a = do
  putStrLn "w mu id xxx"
  print $ mu (w mu id xxx)
  putStrLn "----"

test9b = do
  putStrLn "w id mu xxx"
  print $ mu (w id mu xxx)
  putStrLn "---"

{-
what the heck is the xxxhc below?
We have to match with bimap2 :: (Functor f, Functor g) => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f')
f = HC MyList MyList
f' = MyList

Now f ~> f' is a natural transformation.

g = MyList (TODO: should be Ident)
g' = MyList

So, g ~> g' is a natural transformation.

b = HC

b g f got to be HC MyList HC MyList MyList

b g' f' will be HC MyList MyList

So, natural transformation from `HC MyList HC MyList MyList` to `MyList MyList` (instantiated on a=Integer in this example.
-}

ten = 10::Integer
xxxhc = HC (Identity (HC (MyCons (MyCons ten MyNil) MyNil)))
muhc:: HC MyList MyList a -> MyList a
muhc (HC l) = mu l
ww = bimap2 muhc id xxxhc

test9 = do
  putStrLn "ww"
  print $ ww
  putStrLn "---"

xxxhcr = HC (HC (MyCons (MyCons (Identity ten) MyNil) MyNil))
wwr = bimap2 id muhc xxxhcr
test9r = do
  putStrLn "wwr"
  print $ wwr
  putStrLn "---"




{-

How do I say mu is natural transformation given:
        mu: MyList (MyList a) -> MyList a

Should I say:

(MyList MyList) -> MyList a, natural transformation instantiated on a ?
This is possible only if we can encode MyList MyList somehow, may be Compose? Or can we use HC somehow?

can i use alias ?

MyDList a = MyList (MyList a)
and mu is from MyDList a -> MyList a

μ is a natural transformation from the square of the functor T2 back to T. The square is simply the functor composed with itself, T ∘ T (we can only do this kind of squaring for endofunctors).



(saying this is wrong: mu: MyList (MyList a) -> Identity (MyList a)  so, it is instantiated on (MyList a) and from functor MyList to functor Identity. Because we are interested in inistatiating it on `a`)

-}

--- end of horiz comp based analysis --


{-

mu defn: mu (MyCons (MyCons a l1) l2) = MyCons a (concat l1 (mu l2))

mu . mu:

mu xxx
mu (MyCons (MyCons (MyCons a l1) l2) l3) = MyCons (MyCons a l1) (concat l2 (mu l3))

mu of above:

MyCons a (concat l1 (mu (concat l2 (mu l3))))

Now mu . fmap mu:


fmap f (MyCons x l) = MyCons (f x) (fmap f l)

fmap mu (MyCons (MyCons (MyCons a l1) l2) l3) = MyCons (mu (MyCons (MyCons a l1) l2)) (fmap mu l3)

MyCons (MyCons a (concat l1 (mu l2))) (fmap mu l3)

mu of above

mu defn: mu (MyCons (MyCons a l1) l2) = MyCons a (concat l1 (mu l2))

MyCons a (concat (concat l1 (mu l2)) (mu fmap mu l3))

So, assoc of bind translates to assoc of concat.
 
-}


{-
Why is bind defined as above is associative operator?
Convert to fish for analysis.
-}

(>=>) :: (a -> MyList b) -> (b -> MyList c) -> (a -> MyList c)
infixl >=>
f >=> g = \a -> let
    l = f a
    in l >>= g

test8 = ( (\v -> rtn v) >=> msqr ) 5

{-
(f >=> g) >=> h

f >=> g = \a -> let 
    l = f a
    in l >>= g

(f >=> g) >=> h = \a -> let 
    l = (f >=> g) a
    in l >>= h

\a -> let
    l = (\v -> let lv = f v in lv >>= g) a
    in l >>= h

\a -> let
    lv = f a
    l = lv >>= g
    in l >>= h

vs.

f >=> (g >=> h)

\a ->  let 
  l = f a
  in l >>= (g >=> h)

\a ->  let 
  l = f a
  in l >>= (\v -> let lv = g v in lv >>= h)

\a ->  let
  l = f a
  in l >>= (\v -> let lv = g v in lv >> h)

-}

main = do
  do {
    putStrLn "Below are simple 1-d lists:";
    print x;
    print y;
    print z;
    putStrLn "------"
  }
  do { test0}
  do { test_assoc xxx }
  do { test_assoc_via_pairs x y z }
  print test4
  print test5
  print test6
  print test7
  print (test6 == test7)
  print test8
  do { test9a }
  do { test9b }
  do { test9 }
  do { test9r }
