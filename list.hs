{-# OPTIONS_GHC -fglasgow-exts #-}

{-

Look at Categories for the working mathematician.  See page 162, eqn (1).
Also see first para page 138 in the same book.

We have 3 things here (1) bifunctor (i.e., a functor that takes two objects from product category C x C and maps to another object in category C), replaced by endofunctor composition - this is monoidal aspect (2) multiplication, the binary operation, replaced by mu which takes a bifunctor as argument and returns an object of C (3) unit element by identity functor to the given functor.

Endofunctor compostion is bifunctor. It satisfies laws of functor (1) maps identity to identity (2) preserves functor composition. Also functor composition is associative so we do not need rho and lambda and associator.

So, take two MyList's to generate combined one `MyList (MyList)`. That is object mapping portion.
For function mapping, it takes two functions f and g and creates g . fmap f.

mu plays the role of multiplication. How?

So mu : MyList x MyList -> MyList

It takes two objects and c

Also read last section/appendix of : http://uu.diva-portal.org/smash/get/diva2:1369286/FULLTEXT01.pdf

-}


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
To understand xxx, the 3ply nested list, compare to this:
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


-- Demonstrate the associativity of concat with examples:


instance (Arbitrary a) => Arbitrary (MyList a) where
  arbitrary = sized mylist
    where mylist 0 = liftM (\v -> MyCons v MyNil) arbitrary
          mylist n | n > 0 =
                 oneof [liftM (\v -> MyCons v MyNil) arbitrary,
                        liftM2 (\v l -> MyCons v l) arbitrary sublist]
            where sublist = mylist (n `div` 2)

--check0 = quickCheck $ (concat (concat (trace (show x) x) y) z) == (concat x (concat y z))
-- parallels a familiar binary operation: (+ (+ a b) c) = (+ a (+ b c))
check0 = quickCheck $ (concat (concat x y) z) == (concat x (concat y z))
check1 = quickCheck $ \x y z ->  (concat (concat x y) z) == (concat (x::MyList Integer) (concat y z))

{-

Can we get rid of interleaving of these variables and doing so also make the reasoning more general?

What do we need ?
To be able to treat all data (x, y, z) as one unit, cartesian product immediately comes to mind. If a function cannot take 2 parameters, what do you do ? shove in a data structure (ADT) and pass it - Any ADT is nothing but cartesian product. Retriving each is getter/projection operation, already. If the ADT is type parametric (like queue / list), then it is also a Functor. If it can operate on more than one type parameter, it is bi- or multi-functor.


We can think of the same bundling mechanism for functions (here just concat repeated).

Finally we also need the ability to split them and apply parts to parts of the function group.

Since here we really do not care what the content of x, y or z, we can use Functor (i.e., type-parametrized data type), specifically Bifunctor.

If you see above, you notice that we want to package two functors (x and y ; or concat x y and z) and apply a function to each item packaged.
For example in lhs, concat (concat x y) z, we first package x and y then package that result with z. Finally we apply concat to first item (packaged x and y) and nothing to the second item (z). Notice that "nothing" is an edge condition and a bothereration. Introduce identity function - this is one advantage of identity functions.

We eventually concat the result. So

package< package<x, y> z>

Apply concat to first packaged item, and nothing to second item (or apply identity).

So we are looking for ability to package two functors and apply two functions: This obviously feels like bifunctor.

We can use MyPair to package.

concat (x, concat (y, z)) = concat (concat (x, y), z)


Also, we have to ask concat to operate on pair instead. Let's call it concatm.

concat (x, concat (y, z)) = concat (concat (x, y), z)
(concat . bimap id concat)(x, (y, z))


First package lhs: MyPair x (MyPair y z)
Apply bimap to it with id on x and concat on enclosed MyPair. bimap automatically lifts id and concat by MyPair to our advantage.

So, the overall lhs becomes:

concat (bimap id concat (MyPair x (MyPair y z)) ). Or in point-free style:

(concat . bimap id concat) (MyPair x (MyPair y z))

Likewise rhs side:

(concat . bimap concat id ) (MyPair (MyPair x y) z)

Is bimap (bifunctor) a side tool used to prove the associativity of concat or is something fundamental going on ?

At a concept level (no matter implemented as pair, or endo(functor) composition), you always can create this artificial layer out of associative
binary operators. So, having a bifunctor is fundamental.

In the bifunctor, since it can store 2 objects, if we create a new bifunctor with x and y (level 1) and yet another that stores the first one along with z (level 2),
we can have it bimap on this composite via mu and id.
An an example, 

MyPair (MyPair x y) z

You can bimap mu and id on it. Note that you cannot switch order to id and mu. So, they are only isomprophic (MyPair is non strict monoidal category). So, we need an associator like `alpha` defined in the test case test_assoc_via_pairs below.

But, but, if you functor composition instead of MyPair, you can switch them, just that mu and id natural components are different (endofunctor composition is strict monoidal category).


More concretely, being a functor, a bifunctor has two parts: (1) maps pair of objects, i.e., maps  x, y to <B a b>. For example a and b to MyPair a b or (a, b)) (2) operates on a pair of morphisms. For example mult, id. (TODO: this is bad explanation, fix it).

-}


class BiFunctor f where
  bimap :: (a->c) -> (b->d) -> f a b -> f c d
  first :: (a->c) -> f a b -> f c b 
  first g = bimap g id
  second :: (b->d) -> f a b -> f a d
  second  = bimap id
  bimap g h = first g . second h
  {-# MINIMAL bimap | first, second #-}


data MyPair a b = MyPair a b

-- Let's define mult operation: takes bifunctor as argument and returns MyList
concatm :: MyPair (MyList a) (MyList a) -> MyList a
concatm (MyPair l1 l2) = concat l1 l2

instance BiFunctor MyPair where
  bimap f g (MyPair a b)  = MyPair (f a) (g b)

{-
Note that BiFunctor is defined as a generic functor, but concatm won't be a viable function for bimap (either as f or g) unless 
the corresponding item stored (a or b) is actually itself is a MyPair of 2 lists.
-}

test_assoc_via_pairs x y z = let

  alpha :: MyPair (MyPair a b) c -> MyPair a (MyPair b c)
  alpha (MyPair (MyPair x y) z) = MyPair x (MyPair y z)

  sample = MyPair (MyPair x y) z
  lhs = (concatm . bimap concatm id) sample
  rhs = (concatm . bimap id concatm) (alpha sample)
  in  (lhs == rhs)

check2 = quickCheck $ \x y z -> (test_assoc_via_pairs (x::MyList Integer) y z)

{-

Do we really need full-blown categorical product like MyPair above to establish associativity? 
We do not use category product and projection operations (i.e., we do not need getters) - we have not used any of these properties of a (cartesean) product.

Also, categorical product is commutative, which we do not need/use: https://en.wikipedia.org/wiki/Product_(category_theory)

We also do not need/use knowledge of what type of data is stored, they may be integers, strings or some complex structures.

Let's look at the essential pieces we used: (1) need to store two functors (x and y or y and z) at different levels ( MyPair x y and z in a single bifunctor) (2) need to apply a natural transformation (concat) to the two functors stored and create a new functor.
Hmm... that sounds like a bifunctor, but specialized for functors and natural transformations.

So, no, we do not need product. All we need is a bifunctor.

All we need is ability to lift 2 natural transformations (mu and id) to prove the associativity and unit laws (mu . bimap id mu = mu . bimap mu id).

So, given that a functor can contain anything (forall, any object), why not trick it to store an instance of itself recursively? Then we do not need a new structure
like MyPair. This is endofunctor composition. But you may ask, don't you need to strore two things for bifunctor but not one in itself? That's where single object
monoid comes into play. With monoids and monads, we are always thinking about the "structural" content than payload of the ADT. For example "maybe", "the state", "the count", "the pair or comma" than what is actually contained. Here the monadic context is collapsed. After all, this is nothing new and happens with all collection type recursive data types like tree, list - they refer to self via recursive data type definition. (BTW: A free monad satisfies all the Monad laws, but does not do any collapsing https://stackoverflow.com/questions/13352205/what-are-free-monads).

And associativity is dealing with this structural aspect only. So when we are saying one functor contains another as payload, we still have two instances
of the "structure" to deal with - and monoid rules help with associativity of manipulating this structures in any order we want with end result being the same.

Now, the mapping from a (endo)functor  to another is a natual transformation. So, our bifunctor has to lift two unrelated natural transformations. So we need
a more general definition of composition of natural transformations than vertical where output of one needs to match input of another. Horizontal composition nicely fits it.  Essentially, the endo-bifunctor, now takes two natural transformations (id and mu) and creates a horizontal composition of these two.

TODO: Now do the same but using horiz comp

-}

{-

We need something less powerful than product and more powerful than a simple functor.
One important thing to keep in mind is that it is not about data (1, 2, 3, 10, 20, 30, 100, 200, 300, 400) itself, but how it is structured in list(s).
So, we are looking for structural power. Enter Functor composition!

One problem above is that we have to create a MyPair version of natural transformation for every functor (for MyList we created concatm paralleling concat).

Can we avoid it?

You might be asking, we are putting two lists in a pair in testing the associativity above, but can we put them in a new list and work our way?
Given that concat is natural transformation, we should be able to instantiate it for MyList or 'a' depending on which side of equation we are talking about.
We also always use id for one of the functions in bimap and could that be avoided as well?

See Monoidal categories section: https://bartoszmilewski.com/2016/12/27/monads-categorically/

We also have to ask concat to operate on List of lists instead of pair.


-}


{-
In the following discussion concat refers to the version which does flatten (the one defined in Haskell than this document).

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

(here underscore means, instantiated at that object).

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

-- define mult operation, taking bifunctor as argument (endofunctor composition), and returning MyList.
concatn :: MyList (MyList a) -> MyList a
concatn MyNil = MyNil
concatn (MyCons l rl) = concat l (concatn rl)


{-
In above concatn, the bifunctor (MyList (MyList)) is not an explicit construct like MyPair, but implicit by by endofunctor composition.
To enable this we need something like HC (defined below).

With respect to concatn, it seems we are replacing cross product (concat x y) with composition (concatn [x, y]).
So, if we want to replace the cross product with composition of endofunctors, we need an equivalent for natural transformations. 
(ref: https://blog.merovius.de/posts/2018-01-08-monads-are-just-monoids/)
Is this what happening here between concat and concatn?

Here like concatm we had to create for MyPair, we have to create a new concatn paralleling concat. But in this case that we do not need 
any new data structure like MyPair, but reuse the same (MyList).

-}

test_assoc aaa = 
  let lhs = (concatn . concatn) aaa
      rhs = (concatn . fmap concatn) aaa
  in  (lhs == rhs)

check3 = quickCheck $ test_assoc xxx
check4 = quickCheck $ \aaa -> (test_assoc (aaa::MyList (MyList (MyList Integer))))



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

test_mu input expected = (mu input == expected)
 
check5 = quickCheck $ test_mu xx (MyCons 3 (MyCons 2 (MyCons 1 (MyCons 30 (MyCons 20 (MyCons 10 MyNil))))))

-- Due to associativity (mu. fmap mu = mu . mu)
-- Let's first specify the components of natural transformations mu that we expect.
mu_mi :: MyList (MyList (MyList Integer)) -> MyList (MyList Integer)
mu_mi = mu
mu_i :: MyList (MyList Integer) -> MyList Integer 
mu_i = mu
-- Using `asTypeOf` to prove that the natural transformation components of mu used are what expected to be
-- You can also use (mu . (mu `asTypeOf` _)) xxx in ghci with main commented out to verify the types.
test_mu_mu_and_mu_fmap aaa = ( ( (mu `asTypeOf` mu_i) . (mu `asTypeOf` mu_mi)) aaa ) == ((mu `asTypeOf` mu_i) . fmap (mu `asTypeOf` mu_i)) aaa

check8 = quickCheck $ test_mu_mu_and_mu_fmap xxx

-- Store the 2 things to be multiplied as f and g (endofunctor composition)
type (g :<*> f) a = g (f a)

-- bimap to apply two functions
(<*>) ::  (Functor a) => (a (a z) -> a (a y)) -> (a x -> a z) -> a (a x) -> a (a y)
(<*>) g f = g . fmap f

-- mu is real mult operation. So store 3 things. And use above bimap to operate on the first two or the last two of the compositon of 3.
law2_left, law2_right :: ( MyList :<*> (MyList :<*> MyList) ) Integer -> MyList Integer
law2_left  = (mu `asTypeOf` mu_i) . ((mu `asTypeOf` mu_mi) <*> id)
law2_right = (mu `asTypeOf` mu_i) . (id <*> (mu `asTypeOf` mu_i))

check101 = quickCheck $ \n -> law2_left n == law2_right (n::MyList (MyList (MyList Integer)))

{-
The type signature of <*> is particularly not enlightening. 
There is a different way to look the <*> operation that sheds more light, particularly as a bifunctor of endofunctor composition 
and its connection to horizontal composition.

First note that, although horizontal composition (which itself is a natural transformation) is generally written as having components 
at an (categorical) object, the constituents have components at different objects. Let's look at the definition:

Let α: F -> F' and β: G -> G' two natural transformations.

(β ∘ α)_a = G' α_a . β_F a = β_F'a . G α_a

Here β is not a-component, but either F a or F' a component where as α is a-component.

-}

-- First define nat. trans constraint as map from functor to functor
type (~>) f g = forall a. f a -> g a


{-
First we need to solve a problem, we need to make Haskell think that MyList MyList is a functor.
This is so that mu :: MyList (MyList a) -> MyList a is considered a function from a functor MyList MyList to MyList,
hence can be treated as the component `a` of the natural transformation.

There is no direct way of doing it. We have to wrap inside a Compose (like HC or FComp or Compose etc.,) and compose composes.

https://stackoverflow.com/questions/25210743/bicategories-in-haskell
https://wiki.haskell.org/Type_composition

-}


data HC g f a = HC { unHC :: g (f a) } deriving (Show)


-- | horizontal composition of natural transformations
hc :: Functor g => (g ~> g') -> (f ~> f') -> ((g `HC` f) ~> (g' `HC` f'))
g `hc` f = HC . g . fmap f . unHC


class NatBiFunctor b where
  -- bimap2 is basically a special case of horizontal composition of two natural transformations (f ~> f') and (g ~> g').
  -- It is special case because all catagories involved are just HASK.
  -- See Monoidal categories section: https://bartoszmilewski.com/2016/12/27/monads-categorically/
  --bimap2 :: (Functor f, Functor g) => (f ~> f') -> (g ~> g') -> (b g f ~> b g' f')
  bimap2 :: (Functor f, Functor g, Functor f', Functor g') => (forall x. (f x -> f' x)) -> (forall y. (g y -> g' y)) -> (forall z. (b g f z -> b g' f' z))

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


f_1 :: MyList a -> MyList a
f_1 x = x
g_1 :: HC MyList MyList a -> MyList a
g_1 = mu . unHC

xxxhclhs = HC (HC (MyCons a (MyCons b (MyCons c MyNil))))

lhs v = mu ( unHC (bimap2 f_1 g_1 v) )


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

Understand how because of endofunctor composition. bimap2 is simplified. 
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
  check101
{-
And we haven't yet talked about mappend:

https://stackoverflow.com/questions/10961483/haskell-duplicated-functions-and-mappend

References:

https://stackoverflow.com/questions/65258061/can-i-print-in-haskell-the-type-of-a-polymorphic-function-as-it-would-become-if
http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html for example how it is done there.
https://www.haskellforall.com/2013/12/equational-reasoning.html
https://stackoverflow.com/questions/29113863/where-does-the-name-of-equational-reasoning-come-from
https://stackoverflow.com/questions/5671271/what-are-advantages-and-disadvantages-of-point-free-style-in-functional-progra
http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html
-}

