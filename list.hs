{-# OPTIONS_GHC -fglasgow-exts #-}

import Prelude hiding((<*>), (>>=), concat)
import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Debug.Trace

-- recursively defined list data structure
data MyList a = MyNil | MyCons a (MyList a) deriving (Show, Eq)

rtn x = MyCons x (MyNil)

-- Note that this concat is different from the one in Haskell List. That one is MyList (MyList a) -> MyList a, i.e., flatten/monad join/mu.
-- This one is simply combining elements from given two lists into one list.
concat :: MyList a -> MyList a -> MyList a
concat l1 MyNil = l1
concat MyNil l2 = l2
concat (MyCons a l1) l2  = MyCons a (concat l1 l2)

-- Some lists to play with
x::MyList Integer
x = MyCons 3 (MyCons 2 (MyCons 1 (MyNil)))

y::MyList Integer
y = MyCons 30 (MyCons 20 (MyCons 10 MyNil))

z::MyList Integer
z = MyCons 300 (MyCons 200 (MyCons 100 (MyNil)))

xx:: (MyList (MyList Integer))
xx = MyCons x (MyCons y MyNil)

a:: MyList (MyList Integer)
a = MyCons (MyCons 1 (MyCons 2 (MyCons 3 MyNil))) MyNil
-- a = [ [1, 2, 3] ]
bb1:: MyList Integer
bb2:: MyList Integer
bb1 = MyCons 10 (MyCons 20 (MyCons 30 MyNil))
-- bb1 = [10, 20, 30]
bb2 = MyCons 100 (MyCons 200 (MyCons 300 MyNil))
-- bb2 = [100, 200, 300]
b = MyCons bb1 (MyCons bb2 MyNil)
-- b = [ [10, 20, 30],  [100, 200, 300] ]
c:: MyList (MyList Integer)
c = MyCons (MyCons 400 MyNil) MyNil
-- c = [ [400] ]
xxx:: MyList (MyList (MyList Integer))
xxx = MyCons a (MyCons b (MyCons c MyNil))

{-
Demonstrate the associativity of concat with examples:

We want to show that:
concat (concat x y) z = concat x (concat y z)

Relation to math:  We use parentheses to indicate computation order (a + b) + c = a + (b + c). This can be thought of +( + (a b) c) so on. 
Also you need a way to refer to (a b) together like a pair passed to function +.

-}

instance (Arbitrary a) => Arbitrary (MyList a) where
  arbitrary = sized mylist
    where mylist 0 = liftM (\v -> MyCons v MyNil) arbitrary
          mylist n | n > 0 =
                 oneof [liftM (\v -> MyCons v MyNil) arbitrary,
                        liftM2 (\v l -> MyCons v l) arbitrary sublist]
            where sublist = mylist (n `div` 2)

--check0 = quickCheck $ (concat (concat (trace (show x) x) y) z) == (concat x (concat y z))
check0 = quickCheck $ (concat (concat x y) z) == (concat x (concat y z))
check1 = quickCheck $ \x y z ->  (concat (concat x y) z) == (concat (x::MyList Integer) (concat y z))

{-
Reasoning about above associativity law is made difficult due to interleaved presence of variables that need to be tracked/accounted for.
See: https://www.haskellforall.com/2013/12/equational-reasoning.html
https://stackoverflow.com/questions/29113863/where-does-the-name-of-equational-reasoning-come-from
Also see: https://stackoverflow.com/questions/5671271/what-are-advantages-and-disadvantages-of-point-free-style-in-functional-progra
And above all see this: http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html

Can we get rid of interleaving of these variables and doing so also make the reasoning more general?

What do we need ?
To be able to separate all data (x, y, z) as one unit:
If you see above, you notice that we want to package two functors (x and y ; or concat x y and z) and apply a function to each item packaged.
For example in lhs, concat (concat x y) z, we first package x and y then package that result with z. Finally we apply concat to first item (packaged x and y) and nothing to the second item (z).

We eventually concat the result. So

package< package<x, y> z>

Apply concat to first packaged item, and nothing to second item (or identity).

So we are looking for ability to package two functors and apply one or two functions: This obviously looks like bifunctor.

We can use MyPair to package.

concat (x, concat (y, z)) = concat (concat (x, y), z)


Also, we have to ask concat to operate on pair instead. Let's call it concatm. 
We have to ask concat to operate on pair instead of List of lists.

concat (x, concat (y, z)) = concat (concat (x, y), z)
(concat . bimap id concat)(x, (y, z))


First package lhs: MyPair x (MyPair y z)
Apply bimap to it with id on x and concat on enclosed MyPair. bimap automatically lifts id and concat by MyPair to our advantage.

So, the overall lhs becomes:

concat (bimap id concat (MyPair x (MyPair y z)) ). Or in point-free style:

(concat . bimap id concat) (MyPair x (MyPair y z))

Likewise rhs side:

(concat . bimap concat id ) (MyPair (MyPair x y) z)

-}

data MyPair a b = MyPair a b

bimap:: (a -> c) -> (b -> d) -> (MyPair a b) -> MyPair c d
bimap f g (MyPair a b)  = MyPair (f a) (g b)

concatm :: MyPair (MyList a) (MyList a) -> MyList a
concatm (MyPair l1 l2) = concat l1 l2

test_assoc_via_pairs x y z = let
  p_lhs = MyPair (MyPair x y) z
  p_rhs = MyPair x (MyPair y z)
  -- note how both (concatm . bimap concatm id) and (concatm . bimap id concatm) are in point-free form indicating, 
  -- they are equal at morphism level instead at object level for each object
  lhs = (concatm . bimap concatm id) p_lhs
  rhs = (concatm . bimap id concatm) p_rhs
  in  (lhs == rhs)

check2 = quickCheck $ \x y z -> (test_assoc_via_pairs (x::MyList Integer) y z)

{-
You might be tempted to make MyPair a functor and lift original concat by its fmap. But this is not going to be neat, what do you return from fmap? Something dummy? It is kludgy.

instance Functor MyPair where
  fmap f (MyPair l1 l2) = MyPair (f l1 l2) <what_can_we_use_here>

We need something less powerful than product and more powerful than a simple functor.
One important thing to keep in mind is that it is not about data (1, 2, 3, 10, 20, 30, 100, 200, 300, 400) itself, but how it is structured in list(s).
So, we are looking for structural power. Enter Functor composition!

-}

{-

concatm (x, concat (y, z)) = concat (concat (x, y), z)

Now, about associativity of concatm

Let, concatm (x, y) = [x, y]

concatm (concatm (x, y), z) = [x, y, z] and so is
concatm (x, concatm (y, z)) = [x, y, z]

-}

{-

One problem above is that we have to create a MyPair version of natural transformation for every functor (for MyList we created concatm paralleling concat).

Can we avoid it?

You might be asking, we are putting two lists in a map in testing the associativity above, but can we put them in a new list and work our way?
Given that concat is natural transformation, we should be able to instantiate it for MyList or 'a' depending on which side of equation we are talking about.
We also always use id for one of the functions in bimap and could that be avoided as well?

See Monoidal categories section: https://bartoszmilewski.com/2016/12/27/monads-categorically/

-}


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

Consider list xxx = [  [[1,2,3]], [[10,20,30],[100,200,300]], [[400]]  ] (spot 1 in the diagram)

concat . fmap concat flattens inner list first, and then flattens the outer list. This is : C+(C+C)
fmap concat xxx -> [  [1,2,3], [10,20,30,100,200,300], [400]   ]   (spot 3)
concat . fmap concat xxx -> [1,2,3,10,20,30,100,200,300,400]       (spot 4)

concat . concat will flatten outer list first and then the inner list. This is : (C+C)+C
concat xxx -> [  [1,2,3], [10,20,30], [100,200,300], [400]  ]   (spot 2)

concat . concat xxx ->  [1,2,3,10,20,30,100,200,300,400]        (spot 4)

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

Here like concatm we had to create for MyPair, we have to create a new concatn paralleling concat. But one difference is that we are not leveraging
any new data structure (like MyPair), but reusing the same (MyList).

-}

test_assoc aaa = 
  let lhs = (concatn . concatn) aaa
      rhs = (concatn . fmap concatn) aaa
  in  (lhs == rhs)

check3 = quickCheck $ test_assoc xxx
check4 = quickCheck $ \aaa -> (test_assoc (aaa::MyList (MyList (MyList Integer))))

{-

Do we really need full-blown categorical product like MyPair above to establish associativity? 
We do not need category product and projection operations and so on as we have not used any of these properties of a (cartesean) product.

Also, categorical product is commutative, which we do not need/use: https://en.wikipedia.org/wiki/Product_(category_theory)

Let's look at the essential pieces we used: (1) need to store two functors (x and y or y and z) at different levels ((x,y) and z) (2) need to apply a natural transformation (concat) to the two functors stored and create a new functor.
Hmm... that sounds like a bifunctor, but specialized for functors and natural transformations.

So, no we do not need product. All we need is a bifunctor. 

All we need is ability to lift 2 natural transformations (mu and id) to prove the associativity and unit laws (mu . bimap id mu = mu . bimap mu id).

So, given that a functor can contain anything (forall, any object), why not trick it to store an instance of itself recursively? The we do not need a new structure
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

mu :: forall a. MyList (MyList a) -> MyList a
mu MyNil = MyNil
{-
  Following step is very important to understanding composing functor vs. as a binary monoid operator operating on two instances of functor.
  The logic and semantics are specific to each monad. List vs Tree vs others.
 
  Based on xx = MyCons x (MyCons y MyNil):
  You need to do mu on l2 as sedond list is enclosed in MyCons
-}
mu (MyCons (MyCons a l1) l2) = MyCons a (concat l1 (mu l2))

test_mu aa bb = (mu aa == bb)
 
check5 = quickCheck $ test_mu xx (MyCons 3 (MyCons 2 (MyCons 1 (MyCons 30 (MyCons 20 (MyCons 10 MyNil))))))

-- bind operator >>=, using mu and fmap
(>>=) :: MyList a -> (a -> MyList b) -> MyList b
infixl >>=
l >>= f = mu (fmap f l)

msqr a = rtn (a*a)

test_bind_op a b = (a >>= msqr) == b

check6 = quickCheck $ test_bind_op x (MyCons 9 (MyCons 4 (MyCons 1 MyNil)))

-- bind, but this time directly without using mu and fmap
bind:: MyList a -> (a -> MyList b) -> MyList b
infixl `bind`
MyNil `bind` _ = MyNil
-- Core definition of monoid (underlying the monad) for List: go through each element of list, apply function to each element that returns a sub-list. Pre-concatenate it with rest of result (recursively). If the lists are nested
-- further, this operation better be associative.
-- is associative.
(MyCons x l) `bind` f = concat (f x) (l `bind` f)

test_bind a b = (a `bind` msqr) == b 
check7 = quickCheck $ test_bind x (MyCons 9 (MyCons 4 (MyCons 1 MyNil)))

-- 3ply nested list
---xxx = MyCons (MyCons (MyCons 3 MyNil) (MyCons (MyCons 2 MyNil) (MyCons (MyCons 1 MyNil) MyNil))) MyNil
{-
xxx = [  [
           [1,2,3]
         ], 
         [
           [10,20,30],[100,200,300]
         ],
         [
           [400]
         ]
      ]
-}

{-

Note that you can use asTypeOf to investigate the instantiation (natural component) of mu in the first and second occurence: mu . fmap mu:

Prelude Main> ((mu `asTypeOf` _) . fmap mu) xxx

<interactive>:2:17: error:
    • Found hole: _ :: MyList (MyList Integer) -> MyList Integer
      ...
          with mu @Integer

Prelude Main> (mu . fmap (mu `asTypeOf` _)) xxx

<interactive>:3:27: error:
    • Found hole: _ :: MyList (MyList Integer) -> MyList Integer
      ...
          with mu @Integer
-}

-- Due to associativity (mu. fmap mu = mu . mu)
test_mu_mu_and_mu_fmap aaa = ( (mu . mu) aaa ) == (mu . fmap mu) aaa

check8 = quickCheck $ test_mu_mu_and_mu_fmap xxx

{-

Prelude Main> (mu . (mu `asTypeOf` _)) xxx

    • Found hole:
        _ :: MyList (MyList (MyList Integer)) -> MyList (MyList Integer)
        ...
        with mu @(MyList Integer)
          
Prelude Main> ((mu `asTypeOf` _) . mu) xxx

<interactive>:3:17: error:
    • Found hole: _ :: MyList (MyList Integer) -> MyList Integer
      ...
      with mu @Integer
-}

-- Now let's use horiz comp
-- First define nat. trans constraint as map from functor to functor
{-
First we need to solve a problem, we need to make Haskell think that MyList MyList is a functor.
This is so that mu :: MyList (MyList a) -> MyList a is considered a function from a functor MyList MyList to MyList,
hence can be treated as the component `a` of the natural transformation.

There is no direct way of doing it. We have to wrap inside a Compose (like HC or FComp or Compose etc.,) and compose composes.

https://stackoverflow.com/questions/25210743/bicategories-in-haskell
https://wiki.haskell.org/Type_composition


-}

type (~>) f g = forall a. f a -> g a


data HC g f a = HC { unHC :: g (f a) } deriving (Show)
type (g :<*> f) a = g (f a)

-- Instead of using HC, consider creating a type alias for an operator (for example, :<*>) to give the visual appeal of the fact that we are dealing with monoid operator, i.e., (g :<*> f) a = g (f a)
-- See http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html for example how it is done there.

-- | horizontal composition of natural transformations
hc :: Functor g => (g ~> g') -> (f ~> f') -> ((g `HC` f) ~> (g' `HC` f'))
g `hc` f = HC . g . fmap f . unHC


--(<*>) :: (Functor a, Functor b, Functor c, Functor d) => (a (d x) -> c (d y)) -> (b z -> d x) -> a (b z) -> c (d y)
-- this works too: (<*>) ::  (Functor a, Functor b) => (a (a x) -> a (a y)) -> (b z -> a x) -> a (b z) -> a (a y)
--(<*>) :: (Functor a, Functor b, Functor c, Functor d) =>  (c y -> d y) -> (a x -> b x) -> c (a x) -> d (b x)
-- this works for rhs_plain only not lhs_plain : (<*>) ::  (Functor a, Functor b) => (a (a x) -> a (a y)) -> (b x -> a x) -> a (b x) -> a (a y)
-- this also works again for rhs_plain only : (<*>) ::  (Functor a) => (a (a x) -> a (a y)) -> (a x -> a x) -> a (a x) -> a (a y)

-- this works for both: (<*>) ::  (Functor a, Functor b) => (a (a z) -> a (a y)) -> (b x -> a z) -> a (b x) -> a (a y)

-- this works too for both since there is only one functor
(<*>) ::  (Functor a) => (a (a z) -> a (a y)) -> (a x -> a z) -> a (a x) -> a (a y)

--(<*>) ::  (Functor a, Functor b) => (c z -> d z) -> (b x -> a x) -> a (b x) -> a (a y)

--(<*>) :: (Functor f, Functor g, Functor f', Functor g') => (forall y. g y -> g' y) -> (forall x. f x -> f' x) -> (forall z. g (f z) -> g' (f' z))

(<*>) g f = g . fmap f

class NatBiFunctor b where
  -- bimap2 is basically a special case of horizontal composition of two natural transformations (f ~> f') and (g ~> g'). 
  -- It is special case because all catagories involved are just HASK.
  -- See Monoidal categories section: https://bartoszmilewski.com/2016/12/27/monads-categorically/
  bimap2 :: (Functor f, Functor g) => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f')

instance NatBiFunctor HC where
    -- basically horizontal composition of f and g. Since there are two functors creating one endofunctor, we need to wrap them in HC.
    bimap2 f g = HC . g . fmap f . unHC

instance (Functor g, Functor f) => Functor (HC g f) where
  fmap h (HC gf) = HC (fmap (fmap h) gf)

test_assoc_horiz_comp aaa = (mu . (id . fmap mu) ) aaa == (mu . (mu . fmap id) ) aaa
check9 = quickCheck $ test_assoc_horiz_comp xxx

{-
what the heck is the xxxhclhs below?
We have to match with bimap2 :: (Functor f, Functor g) => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f') for the first case (ww)
f = HC MyList MyList
f' = MyList

Now f ~> f' is a natural transformation.

g = Identity
g' = Identity

So, g ~> g' is a natural transformation.

b = HC

b g f got to be HC Identity HC MyList MyList

b g' f' will be HC Identity MyList

So, natural transformation from `HC Identity HC MyList MyList` to `HC Identity MyList`,  instantiated on a=Integer in this example.
bimap2 f g = HC . g . fmap f . unHC

-}

{-
can we verify actual instantiation of mu via something like:

https://stackoverflow.com/questions/65258061/can-i-print-in-haskell-the-type-of-a-polymorphic-function-as-it-would-become-if

-}

f_1 :: MyList a -> MyList a
f_1 x = x
g_1 :: HC MyList MyList a -> MyList a
g_1 = mu . unHC

ten = 10::Integer
--xxxhclhs = HC (HC (MyCons (MyCons (MyCons ten MyNil) MyNil) MyNil))
xxxhclhs = HC (HC (MyCons a (MyCons b (MyCons c MyNil))))

{-
can we use xxx instead of above ?
xxxhclhs = HC (Identity (HC xxx))
-}

lhs v = mu ( unHC (bimap2 f_1 g_1 v) )

lhs_plain = mu . (id <*> mu)

test101 = do
  putStrLn "test101"
  print $ xxx
  print $ lhs_plain xxx
  putStrLn "-------"

{-

Prelude Main> (HC . ((mu `asTypeOf` _) . unHC) . fmap id . unHC) xxxhclhs

    • Found hole:
        _ :: MyList (MyList (MyList Integer)) -> MyList (MyList Integer)
          ...
          with mu @(MyList Integer)

In its full glory, for lhs:

Prelude Main> (mu `asTypeOf` _) (unHC ((HC . ((mu `asTypeOf` _) . unHC) . fmap id . unHC) xxxhclhs))

<interactive>:8:16: error:
    • Found hole: _ :: MyList (MyList Integer) -> MyList Integer
    • In the second argument of ‘asTypeOf’, namely ‘_’
      In the expression: mu `asTypeOf` _
      In the expression:
        (mu `asTypeOf` _)
          (unHC
             ((HC . ((mu `asTypeOf` _) . unHC) . fmap id . unHC) xxxhclhs))
    • Relevant bindings include
        it :: MyList Integer (bound at <interactive>:8:1)
      Valid hole fits include
        concatn :: forall a. MyList (MyList a) -> MyList a
          with concatn @Integer
          (imported from ‘Main’ (and originally defined at list.hs:258:1-7))
        mu :: forall a. MyList (MyList a) -> MyList a
          with mu @Integer
          (imported from ‘Main’ (and originally defined at list.hs:317:1-2))

<interactive>:8:48: error:
    • Found hole:
        _ :: MyList (MyList (MyList Integer)) -> MyList (MyList Integer)
    • In the second argument of ‘asTypeOf’, namely ‘_’
      In the first argument of ‘(.)’, namely ‘(mu `asTypeOf` _)’
      In the first argument of ‘(.)’, namely ‘((mu `asTypeOf` _) . unHC)’
    • Relevant bindings include
        it :: MyList Integer (bound at <interactive>:8:1)
      Valid hole fits include
        concatn :: forall a. MyList (MyList a) -> MyList a
          with concatn @(MyList Integer)
          (imported from ‘Main’ (and originally defined at list.hs:258:1-7))
        mu :: forall a. MyList (MyList a) -> MyList a
          with mu @(MyList Integer)
          (imported from ‘Main’ (and originally defined at list.hs:317:1-2))

-}

{-
in rhs case: 

f = Identity
f' = Identity

so f ~> f' is a natural transformation.

g = HC MyList MyList
g' = MyList

so g ~> g' is a natural transformation.

b = HC

b g f got to be HC (HC MyList MyList) Identity
b g' f' got to be HC MyList Identity.


Final natural transformation is from `HC (HC MyList MyList) Identity` to `HC MyList Identity`,  instantiated on a=Integer in this example.

Here the (MyCon (MyCon ...)) is handled by mu and instantiated at 'Identity Int`.

-}

f_2 :: HC MyList MyList a -> MyList a
f_2 = mu . unHC
g_2 :: MyList a -> MyList a
g_2 x = x

xxxhcrhs =  HC (MyCons (HC a) (MyCons (HC b) (MyCons (HC c) MyNil)))
{-
How did I get xxxhcrhs?
        ------ a  ---, -------------  b ----------------- , --- c----
xxx = [ [ [1, 2, 3] ], [ [10, 20, 30],  [100, 200, 300] ] , [ [400] ] ]

So, to get  HC MyList (HC MyList MyList) Integer:

     HC [ HC [[1, 2, 3] ], HC [ [10, 20, 30],  [100, 200, 300] ] , HC [ [400] ] ] 
     HC (MyCons (HC a) (MyCons (HC b) (MyCons (HC c) MyNil)))

Notice how this paralles: concat (concat x y) z = concat x (concat y z), +(+(a b) c) and needing a way to hold a pair.

-}

rhs v = mu (unHC (bimap2 f_2 g_2 v))
rhs_plain = mu . (mu <*> id)

test102 = do
  putStrLn "test102"
  print $ xxx
  print $ rhs_plain xxx
  putStrLn "-------"

test_assoc_via_bimap_aka_horiz_comp aaa bbb = lhs aaa == rhs bbb

check10 = quickCheck $ test_assoc_via_bimap_aka_horiz_comp xxxhclhs xxxhcrhs

{-
Prelude Main> (HC .  id . fmap ((mu `asTypeOf` _) . unHC) . unHC) xxxhcrhs

    • Found hole: 
        _ :: MyList (MyList Integer) -> MyList Integer
        ...
          with mu @Integer

In its full glory for rhs:

Prelude Main> (mu `asTypeOf` _) (unHC ((HC .  id . fmap ((mu `asTypeOf` _) . unHC) . unHC) xxxhcrhs))

<interactive>:1:16: error:
    • Found hole: _ :: MyList (MyList Integer) -> MyList Integer
    • In the second argument of ‘asTypeOf’, namely ‘_’
      In the expression: mu `asTypeOf` _
      In the expression:
        (mu `asTypeOf` _)
          (unHC
             ((HC . id . fmap ((mu `asTypeOf` _) . unHC) . unHC) xxxhcrhs))
    • Relevant bindings include
        it :: MyList Integer (bound at <interactive>:1:1)
      Valid hole fits include
        concatn :: forall a. MyList (MyList a) -> MyList a
          with concatn @Integer
          (imported from ‘Main’ (and originally defined at list.hs:258:1-7))
        mu :: forall a. MyList (MyList a) -> MyList a
          with mu @Integer
          (imported from ‘Main’ (and originally defined at list.hs:317:1-2))

<interactive>:1:59: error:
    • Found hole: _ :: MyList (MyList Integer) -> MyList Integer
    • In the second argument of ‘asTypeOf’, namely ‘_’
      In the first argument of ‘(.)’, namely ‘(mu `asTypeOf` _)’
      In the first argument of ‘fmap’, namely
        ‘((mu `asTypeOf` _) . unHC)’
    • Relevant bindings include
        it :: MyList Integer (bound at <interactive>:1:1)
      Valid hole fits include
        concatn :: forall a. MyList (MyList a) -> MyList a
          with concatn @Integer
          (imported from ‘Main’ (and originally defined at list.hs:258:1-7))
        mu :: forall a. MyList (MyList a) -> MyList a
          with mu @Integer
          (imported from ‘Main’ (and originally defined at list.hs:317:1-2))
-}

{-
All in all, with "o" standing for horizontal composition:

mu_a . mu_Ta = mu_a . fmap mu_a = mu_a . (mu_a o id)  = mu_a . (bimap2 mu_a id)
                                = mu_a . (id o mu_Ta) = mu_a . (bimap2 id   mu_Ta)

-}

test_assoc_all plain aaa bbb =
  let v1 = (mu . mu) plain
      v2 = (mu . fmap mu) plain
      v3 = (mu . unHC) (((bimap2 id (mu . unHC)) ) aaa)
      v4 = (mu . unHC) (((bimap2 (mu . unHC) id) ) bbb)
  in  (v1 == v2) && (v1 == v3) && (v1 == v4) 

check11 = quickCheck $ test_assoc_all xxx xxxhclhs xxxhcrhs

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

{-
We have not talked about bimap2, but it is a horizontal composition (do `asTypeOf` on it to get details and process them. Summary:

bimap2 :: (Functor f, Functor g) => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f')
bimap2 f g = HC . g . fmap f . unHC

bimap2 id mu 

Actual type: (MyList a -> MyList a)
             -> (HC MyList MyList a -> MyList a)
             -> HC (HC MyList MyList) MyList Integer
             -> HC MyList MyList Integer

Let:
f = f' = MyList 
g = MyList MyList (also a functor as it is composition of functors)
g' = MyList
a = Integer

Then above becomes (Ignoring HC):
   (f a -> f' a) -> (g a -> g' a) -> g (f a) -> g' (f' a)
Or
   (f ~> f') -> (g ~> g') -> (g f ~> g' f')

So bimap2 is a horizontal composition of natural transformations as promised.

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

test_fish = ( (\v -> rtn v) >=> msqr ) 5

check12 = quickCheck $ test_fish == (MyCons 25 MyNil)

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
  do {
    putStrLn "Below is a d-d list:";
    print xxx;
    putStrLn "------"
  }
  check0
  check1
  check2
  check3
  check4
  check5
  check6
  check7
  check8
  check9
  check10
  check11
  check12
  do { test101 }
  do { test102 }

{-
And we haven't yet talked about mappend:

https://stackoverflow.com/questions/10961483/haskell-duplicated-functions-and-mappend

-}

