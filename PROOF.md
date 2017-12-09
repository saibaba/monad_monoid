Statements
----------

Endofunctor category with functor composition as bifunctor is a (strict) monoidal category.

A monad in category C is a monoid in the category of endofunctors of C where product is the composition of endofunctors and unit is identity endofunctor.

Fundamental idea : <strong>associativity</strong>. For example, consider arithmetic multiplication by self (powers):

<pre>
  m * (m * (m * (m * m))) = ((m * m) * (m * m)) * m and various other forms as long as <strong>order on the page 
  (not the execution order)</strong> is maintained. For example, by labeling:

  m<sub>1</sub> * (m<sub>2</sub> * (m<sub>3</sub> * (m<sub>4</sub> * m<sub>5</sub>))) = ((m<sub>1</sub> * m<sub>2</sub>) * (m<sub>3</sub> * m<sub>4</sub>)) * m<sub>5</sub>.
</pre>

Another example with string concatenation:

<pre>
  (m ++ k) ++ h == m ++ (k ++ h)
</pre>


Yet another example from middle school:

<pre>

f1(x) = x + 1
f2(x) =  x^2+x+2
f3(x) = 2 x + 1
f4(x) = x^2

f(x) = (f1 o f2 o f3 o f4) (x) = (f1 o (f2 o f3) o f4) (x) = f1 o (f2 o (f3 o f4(x))) = and numerous other forms.

Here,

f2 o f3 = (2 x + 1)^2 + (2 x + 1) + 2
and so on...

</pre>
 
Legend
------

<pre>

If F is a functor, F<sub>o</sub> maps an object from the source category to target category.
            and    F<sub>m</sub> maps an arrow from the source category to target category.

.   is composition in a category.

∘   is functor composition, sometime, just juxtaposition.

⊗   bifunctor.

I some times use mu = α = μ.

Also η  = eta
     
Todo:

Edit with consistent function and composition symbols (α should used as associator).

</pre>

Intuition
---------

Consider 2 kleisli arrows:
<pre>
f : a -> T<sub>o</sub> b
g : b -> T<sub>o</sub> c
</pre>

How do you compose f with g ?

Since T is a functor, we can lift g and then compose.

<pre>
T<sub>m</sub> g :: T<sub>o</sub> b -> T<sub>o</sub> (T<sub>o</sub> c)
or
T<sub>m</sub> g :: T<sub>o</sub> b -> T<sub>o</sub><sup>2</sup> c
</pre>

But, now you have to get rid of T<sub>o</sub><sup>2</sup>.

What we need is a function:
<pre>
mu: T<sub>o</sub><sup>2</sup> x -> T<sub>o</sub> x
</pre>

So, the result is:
<pre>
gf = mu . T<sub>m</sub> g . f :: a -> T<sub>o</sub> c
</pre>

Now let's say we created a new kleisli arrow:
<pre>
h : c -> T d
</pre>

And we want to compose h with gf. We apply same idea of lifting.

<pre>
T<sub>m</sub> h :: T<sub>o</sub> c -> T<sub>o</sub> (T<sub>o</sub> d)
or
T<sub>m</sub> h :: T<sub>o</sub> c -> T<sub>o</sub><sup>2</sup> d
</pre>

So, the result is:
<pre>
hgf = mu . T<sub>m</sub> h . gf
or
hgf = mu . T<sub>m</sub> h . mu . T<sub>m</sub> g . f :: a -> T<sub>o</sub> d
</pre>

In above you notice that reduction of T powers is done in this order <strong>T<sub>o</sub> ( T<sub>o</sub> (T<sub>o</sub>) )</strong>.

Now, let's say you start with these 2 kleisli arrows in your program:

<pre>
g : b -> T<sub>o</sub> c
h : c -> T<sub>o</sub> d
</pre>

How do you compose them?

<pre>
T<sub>m</sub> h :: T<sub>o</sub> c -> T<sub>o</sub> (T<sub>o</sub> d)
or
T<sub>m</sub> h :: T<sub>o</sub> c -> T<sub>o</sub><sup>2</sup> d
</pre>

So,
<pre>
hg = mu . T<sub>m</sub> h . g :: b -> T<sub>o</sub> d
</pre>

Now let's say that later you decide to compose hg with f.

First lift/fmap hg:

<pre>
hgc = T<sub>m</sub> hg :: T<sub>o</sub> b -> T<sub>o</sub><sup>2</sup> d
</pre>

And then compose with f and reduce functor power with mu:
<pre>
hgc = mu . T<sub>m</sub> hg . f :: a-> T<sub>o</sub> d
or
hgc = mu . T<sub>m</sub> (mu . T<sub>m</sub> h . g) . f :: a-> T<sub>o</sub> d

</pre>

In above you notice that reduction of T powers is done in this order <strong>( T<sub>o</sub> ( T<sub>o</sub> ))  T<sub>o</sub></strong>.

So,

We want associativity of object mapping of endofunctor composition, which turns out to be the case.

One more thing:

Because of identity endofunctor, any functor in endofunctor category can be treated as a natural transformation:

<pre>
Io(a) = a

Hence Io a = id<sub>a</sub>
Or
I = id
</pre>

So a kleisli arrow like f: a -> T b can be written as a nat. trans.

<pre>
fn : I a -> T b

fn(a) = f(a)

</pre>


Also, remember the definition of bifunctor in the endofunctor category [C,C] (objects=endofunctors; morphisms = natural transformations).

<pre>
bimap :: (F -> F') -> (G -> G') -> (F ∘ G -> F' ∘ G')
With F = T<sub>o</sub> ∘ T<sub>o</sub>
     F'= T<sub>o</sub>
     G = I<sub>o</sub>
     G'= T<sub>o</sub>
     F ∘ G = T<sub>o</sub> ∘ T<sub>o</sub> ∘ I<sub>o</sub> = T<sub>o</sub> ∘ T<sub>o</sub>
     F' ∘ G' = T<sub>o</sub> ∘ T<sub>o</sub>

bimap mu fn = mu . T<sub>m</sub> fn = T<sub>o</sub> a -> T<sub>o</sub> b
</pre>


Let's do the same again with-in categorical setting:

First, define 2 abbreviations:
<pre>
bimap mu fn = mu . T<sub>m</sub> fn
gn <> fn = (bimap join gn) . fn
</pre>

Here bimap expects two functions which are natural transformmations. Because of above Identity functor a -> T<sub>o</sub> b can be treated as a natural transformation.

Then redo above steps:

<pre>

f : a -> T b
g : b -> T c
h : c -> T d


f : a -> T b

T<sub>m</sub> g : T<sub>o</sub> b -> T<sub>o</sub> (T<sub>o</sub> c)

r1 = T<sub>m</sub> g . f : a -> T<sub>o</sub> (T<sub>o</sub> c)

r2 = mu . r1 : a -> T1 c

Here T1 stands for T<sub>o</sub> resulted out of reduction of T<sub>o</sub> (T<sub>o</sub>) by mu.

T<sub>m</sub> h : T1 c -> T1 (T<sub>o</sub> d)

r3 = T<sub>m</sub> h . r2 : a -> T1 (T<sub>o</sub> d)

lhs = mu . r3 : a -> Tl d

Here Tl stands for reduction of T1 (T<sub>o</sub>) by mu.
Hence Tl corresponds to reduction order (TxT)xT.

</pre>

Unrolling lhs and simplifying using above abbreviations:

<pre>
lhs = mu . (T<sub>m</sub> h . (mu . (T<sub>m</sub> g . f)))

lhs = h <> (g <> f)
</pre>

<strong>Hence h <> (g <> f) corrsponds to (TxT)xT.</strong>

Now try this way:

<pre>
g : b -> T<sub>o</sub> c
T<sub>m</sub> h : T<sub>o</sub> c -> T<sub>o</sub>(T<sub>o</sub> d)

r4 = T<sub>m</sub> h . g : b -> T<sub>o</sub> (T<sub>o</sub> d)

r5 = mu . r4 : b -> T2 d

Here T2 corrsponds to T<sub>o</sub> resulted out of reduction of T<sub>o</sub> (T<sub>o</sub>) by mu.

Now decide to compose with f:

T<sub>m</sub> r5 : T<sub>o</sub> b -> T<sub>o</sub> (T2 d)

r6 = T<sub>m</sub> r5 . f : a -> T<sub>o</sub> (T2 d)

rhs = mu . r6  : T<sub>o</sub> -> Tr d

Here Tr stands for reduction of (T<sub>o</sub>) T2) by mu.
Hence Tr corresponds to reduction order  Tx(TxT).
</pre>

Unrolling rhs and simplifying using above abbreviations:
<pre>
rhs = mu . ( T<sub>m</sub> (mu . (T<sub>m</sub> h . g) )  ) . f

rhs = mu . T<sub>m</sub> (h <> g)  . f

rhs = (h <> g) <> f

</pre>

<strong>Hence (h <> g) <> f corrsponds to Tx(TxT).</strong>

We expect lhs = rhs

So, Tl = Tr

So (TxT)xT = Tx(TxT)

Composition must be associative.


A sample in Haskell:

<pre>
{-# LANGUAGE RankNTypes #-}
import Control.Monad(join)
import Test.QuickCheck


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

-- this precisely correspond to the diagrams in https://bartoszmilewski.com/2016/12/27/monads-categorically/
lhs_id n = (bimap id join (Just (Just (Just n))))
rhs_id n = (bimap join id (Just (Just (Just n))))
chk2 :: Int -> Bool
chk2 n = lhs_id n == rhs_id n
check2 = quickCheck $ \n -> chk2 n

main = do
  check1
  check2

</pre>

Now, what is the use of this associativity and putting functions in a -> T b form ?

Two uses already:

* Every function further down has access to various variables at previous level.
* T can be a Monad like Maybe or Either could be used to handle errors and/or terminate prematurely the chain of calls.

See, this example:

<pre>

-- naive
wordCount = map (head &&& length) . group . sort . words . map toLower

-- Kleiski arrows
mToLower t = Identity ( map toLower t)
mWords t = Identity (words t)
mSort l = Identity (sort l)
mGroup l = Identity (group l)
mFreq g = Identity (map (head &&& length) g)

-- re-arrange for easy handling
-- m a -> (a -> m b) -> m b
mv <=> f = (bimap join f) mv

wordCount mtext = mtext <=> mToLower <=> mWords <=> mSort <=> mGroup <=> mFreq
-- associativity allows to do this to above as long as  order on the page is maintained
wordCountAssoc  mtext = mtext <=> (mToLower <=> (mWords <=> (mSort <=> (mGroup <=> mFreq))))

-- expand with lambdas
wordCountExp mtext = mtext <=> mToLower <=> (\lt -> ( (mWords lt) <=> (\w -> ((mSort w) <=> (\sw -> ((mGroup sw) <=> (\g -> (mFreq g))))))))

-- Notice, every function further down has access to various variables at previous level.
-- Instead of Identity, a more complex Monad like Maybe or Either could be used to handle errors and/or terminate prematurely

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

</pre>

Introduction
-------------

Consider a category C with objects like a, b and morphisms f::a->b. In Hask, a, b etc., are types like Bool, Integer, Maybe, [], [Maybe], Maybe [] and so on.

Consider its endofunctors like F, i.e.,

<pre>
  F<sub>o</sub> :: C -> C
  F<sub>m</sub> :: (a->b) -> (F<sub>o</sub> a -> F<sub>o</sub> b)
</pre>

And natural transformations between them. i.e.,  F -> G where F and G are endofunctors.

Also, recall the following naturality condition:

<pre>
Given two functors F, G.
Given a morphism f: a->b

Consider a natural transformation α::F->G and its components like α<sub>a</sub> :: F a -> G a.

α<sub>b</sub> . F<sub>m</sub> f = G<sub>m</sub> . α<sub>a</sub>

</pre>

Endofunctor category [C,C]
--------------------------

<pre>
  Objects: endofunctors like F, G above
  Morphisms: components of natural transformations, mu<sub>a</sub>, eta<sub>a</sub>, ...
  Composition: morphisms i.e., components of natural transformations compose as usual (vertical composition of natural transformations)
  Associativity: Composition is associative as it just normal function composition of morphisms(compoents of natural transformations).
  Identity: Identity natural transformation, I is the identity morphism.
</pre>

Identity natural transformation could, simply, be:

<pre>
  I = id
  I<sub>T</sub> = id<sub>T</sub> (for a given Functor, T)
</pre>


Functor composition
-------------------

  The functor composition (not to be confused with the categorical composition defined for above category [C,C]) can be used as bifunctor.

  Functor composition is defined as:

<pre>
  F:: C -> D, G: D -> E be two functors across 3 categories C, D, E.  Composition of F and G GF:: C -> E is defined by:

  For all objects, a of C:: GF<sub>o</sub> a = G<sub>o</sub> (F<sub>o</sub> a)
  For all morphisms, f of C:: GF<sub>m</sub> f = G<sub>m</sub> (F<sub>m</sub> f)
</pre>

  This composition always exists for any two arbitrary functors as you can see from above definition.

  Functor composition, GF is also a functor (preserves the structure). Here is a proof:

<pre>
  Let f, g be morphisms in C and say, g ∘ f  is defined.

  GF<sub>m</sub> (g∘f) = G<sub>m</sub> (F<sub>m</sub> (g∘f))           (defn of composite functor, above)
           = G<sub>m</sub> ( (F<sub>m</sub> g) o (F<sub>m</sub> f)  )  (F is a functor)
           = G<sub>m</sub> (F<sub>m</sub> g) o G<sub>m</sub> (F<sub>m</sub> f)    (G is a functor)
           = GF<sub>m</sub> g o GF<sub>m</sub> f           (defn of composite functor, above)
</pre>

  Functor composition is also associative. Here is a proof:

<pre>

  Let C, D, E, F be 4 categories.
  F :: C->D, G :: D->E, H :: E->H be 3 functors.

  Need to prove: H (GF) = (HG) F, where GF is composite functor of F and G and HG is composite functor of G and H.

  For an object a of C:

  H<sub>o</sub> (GF<sub>o</sub>) a = H<sub>o</sub> (GF<sub>o</sub> a)
                                   = H<sub>o</sub> (G<sub>o</sub> (F<sub>o</sub> a))
                                   = HG<sub>o</sub> (F<sub>o</sub> a)
                                   = (HG<sub>o</sub>) F<sub>o</sub> a

  Likewise for a morphism, f of C,

  H<sub>m</sub> (GF<sub>m</sub>) a = H<sub>m</sub> (GF<sub>m</sub> a)
                                   = H<sub>m</sub> (G<sub>m</sub> (F<sub>m</sub> a))
                                   = HG<sub>m</sub> (F<sub>m</sub> a)
                                   = (HG<sub>m</sub>) F<sub>m</sub> a

</pre>

Monoidal Category
-----------------

A monoidal category has an extra <strong>structure</strong> on top of a regular category. Additional structure is achieved through composite functors or category product and so on.

They way this is done is via a functor, called bifunctor.

Think of bifunctor taking two objects and returning another object all from the same category. For example it can take two objects and return a pair (assuming all pairs are also in the category). Another example is composite functor (one inside another like Maybe [] or [Maybe].

This is represented using ⊗.

It assigns to each pair of objects (i.e., takes two objects as input/arguments) an object a⊗b, of the same category and returns this object. Returned object is a pair or composite functor.

<pre>
⊗ :: C X C -> C   

(here X means it takes two arguments while ⊗ itself could be category product or composite functor).

&lt;a, b&gt; |-> a⊗b
</pre>

Since it is a functor, it takes two  morphisms and returns a new arrow (again, of the same same category). Result is a lefted version of  the function such that the resulting arrow  stores both of the input arrows such that each operates on its object counter part in the structure.

<pre>
&lt;f, g&gt; |-> f ⊗ g 

f  ⊗ g is of type <strong>a ⊗ b -> c ⊗ d</strong> where f: a-> c and g : b -> d.
</pre>

Endofunctor composition is a bifunctor
--------------------------------------

Endofunctor composition creates a structure suitable for forming a monoidal category from an endofunctor category: one functor inside other, like <strong>F<sub>o</sub> a</strong> inside 
   <strong>G<sub>o</sub> (F<sub>o</sub> a)</strong> both being objects.

Since functor composition is associative (in its object composition), this bifunctor makes the monoidal category strict.
Let's denote this bifunctor by ⊗ and the underlying functor composition by ∘ or juxtaposition for brevity.

It lifts two objects (functors) through functor composition:

<pre>
 G<sub>o</sub> ⊗<sub>o</sub> F<sub>o</sub> = G<sub>o</sub> (F<sub>o</sub>) = GF<sub>o</sub>
 
 For F and G two functors/objects:
 G⊗F = G o F = GF

</pre>

It lifts two morphisms (components of natural transformations) as given below.

It lifts, say two morphisms (both of which are the morphisms in the endofunctor category which are nothing 
  but the natural transformations) so that these two operate on their 
  respective parts (<strong>F<sub>o</sub> a</strong> and <strong>G<sub>o</sub> (F<sub>o</sub> a)</strong>)
  stored in the composite endofunctor structure:

<pre>
α:: F<sub>o</sub>->F'<sub>o</sub>
β:: G<sub>o</sub>->G'<sub>o</sub>
</pre>

Lifting of above two by endofunctor composition creates a new mapping:

<pre>
  GF<sub>o</sub> a -> G'F'<sub>o</sub> a
</pre>

<pre>
  
  α ⊗<sub>m</sub> β = bimap α β (let's say bimap = prefix version of ⊗<sub>m</sub>)

  First start by noting that:
  
  G<sub>m</sub> α :: G<sub>o</sub> F<sub>o</sub> -> G<sub>o</sub> F'<sub>o</sub>
  β component used in the following is β<sub>F'<sub>o</sub></sub> 

  bimap α β (G<sub>o</sub> (F<sub>o</sub>))
    = β ( G<sub>o</sub> (G<sub>m</sub> α (F<sub>o</sub>)) )
    = β ( G<sub>o</sub> (F'<sub>o</sub>) )
    = G'<sub>o</sub> (F'<sub>o</sub>)

  To be super accurate, we have to use components of α, and β.
</pre>
 
Note that above also means that the function resulted out of <strong>lifed functions (bimap)</strong> is nothing but 
the horizontal composition of natural transformations, α and β. For example,

<pre>

  From the definition of functor composition, G<sub>o</sub> F<sub>o</sub> = GF<sub>o</sub>.

  bimap α β = β<sub>F'<sub>o</sub></sub> ∘ G<sub>m</sub> α

  or, equivalently

  bimap α β = G'<sub>m</sub> α ∘ β<sub>F<sub>o</sub></sub>

</pre>

What about GF<sub>m</sub> f?

A monoid
--------

A monoid in the from the above (strict) monoidal category is defined by:

Take a single object/functor (hence 'mono'), M from above endofunctor category.

Define two morphisms:

<pre>
    μ :: M ⊗ M -> M
    η :: I -> M
</pre>

  μ is a natural transformation that takes a composite endofunctor (MM<sub>o</sub>) as input and returns an endofunctor (M<sub>o</sub>) as a result.
  μ ⊗ I<sub>M</sub> (or equivalent bimap μ I<sub>M</sub>) is lefted version of μ and I<sub>M</sub> to operate on (M ⊗ M) and M respectively.
  Or I<sub>M</sub> operates on the inner part (M) while μ operates on outerpart (M ⊗ M).
  
  bimap μ I<sub>M</sub> ( (M ⊗ M) ⊗ M ) = bimap μ I<sub>M</sub> (MM<sub>o</sub> (M<sub>o</sub>)) =
     μ (MM<sub>m</sub> (I<sub>M</sub> M<sub>o</sub>))
     μ (MM<sub>m</sub> (I<sub>M</sub> M<sub>o</sub>))
     

μ ⊗ id acts on ( (M ⊗ M) ⊗ M ) such that μ acts on (M ⊗ M) and id<sub>M</sub> acts on M resulting in M ⊗ M. Here the component of μ used is μ<sub>M a</sub> for some a in C.

id ⊗ μ acts on ( M ⊗ (M ⊗ M )) such that μ acts on (M ⊗ M) and id<sub>M</sub> acts on M resulting in M ⊗ M. Here the component of μ used is μ<sub>a</sub> for some a in C.

Underlying structure construction for two objects (M and M) is functor composition which is associative. Hence ( (M ⊗ M) ⊗ M ) = ( M ⊗ (M ⊗ M) ).

Let,

I<sub>M</sub> :: M -> M

η ⊗ I<sub>M</sub> acts on I ⊗ M resulting in M<sup>2</sup> which can be reduced to M by further application of μ.

I<sub>M</sub> ⊗ η acts on M ⊗ I resulting in M<sup>2</sup> which can be reduced to M by further application of μ.

So, both associativity and left/right unitor laws apply.


A monad
-------

<pre>
  if you rename μ = join
            and η = return
</pre>

M is a monad !

Monoid laws translate directly into monad laws.

List them and prove them.

So, "A monad is a monoid in the category of endofunctors".


Side Bar 1
----------
This section has some notes on Identity endofunctor: I. By definition it sends all objects and morphisms of a category to themselves.

<pre>

I<sub>o</sub> a = a
I<sub>m</sub> f = f
</pre>

Proof that above is a functor:
<pre>
I<sub>m</sub> id<sub>a</sub> = id<sub>a</sub> = id<sub>I<sub>o</sub> a</sub>

I<sub>m</sub> (g . f) = g . f = I<sub>m</sub> g . I<sub>m</sub> f

</pre>

Side Bar 2
----------

Translation between join and bind:

<strong>bind</strong> using <strong>join</strong>:
<pre>
bind :: (Monad m) => m a -> (a -> m b) -> m b
bind mv mf = join $ fmap mf mv
</pre>

<strong>join</strong>  using <strong>bind</strong>:
<pre>
join:: m (m a) -> m a
join mma = bind mma id
</pre>

Now do the same with composition of Kleisli arrows (>=>). Because, these is where you see "associativity" in action:

(f >=> g) >=> h ≡ f >=> (g >=> h)

Side Bar 3
----------

First some definitions:

<pre>

unit :: a -> M a
unit v = M v

Define <strong>bind</strong> and <strong>>=></strong> for any two kleisli arrows p :: a -> M b, q :: b -> M c:

bind p = join (fmap p)

>=> p q = (bind q) . p

</pre>

Proof of  left identity for kleisli operator, >=>

<pre>

bind f . unit = f

or

unit >> bind f = f

hence

unit >=> g = unit >> bind g = g

This unit is left identity for kleisli >=>.

</pre>


Proof of right identity 

<pre>

1-(M a) =  id indexed by M a.


Now,

since unit a = M a

bind unit  = join . fmap unit :: M a -> M a

or

bind unit  = <id instantiated for M a> = 1-(M a)


Hence,

g >=> unit = g >> bind unit = g >> 1-(M a) = g

So, unit is right identity for kleisli >=>.

</pre>



Proof of associativity of kleisli operator, >=>

<pre>

Consider 3 kleisli arrows:

f :: a -> M b
g :: b -> M c
h :: c -> M d



(f>=> g) >=> h = (bind g . f) >=> h
               = (bind h) . (bind g . f)
               = bind h . bind g . f       a to M b to M c to M d or a -> M d
               = (bind h. bind g) . f      since . (function composition) is associative
               = bind (bind h . g) . f
               = f >=> (bind h . g)
               = f >=> (g >=> h)

Prove the same using join instead of bind:

bind f = join (fmap f)


</pre>

