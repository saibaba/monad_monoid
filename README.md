Statements
----------

Experiments to understand: "A monad is just a monoid in the category of endofunctors, what's the probleⅿ?"

See NOTES.md as well.

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

Don't you wish you had the same level of flexibility when writing programs? Like, below where each function could continue the flow or terminate prematurely due to various conditions?

<pre>
wordCount = map (head &&& length) . group . sort . words . map toLower
</pre>

Or any of these combinations if you already have some functions grouped already in a previous project due to other needs:

<pre>
wordCount = map (head &&& length) . ( group . sort ) . words . map toLower
</pre>

Or

<pre>
wordCount = map (head &&& length) . ( group . ( sort  . ( words . (map toLower))))
</pre>

Or even more control:

<pre>
wordCount = map (head &&& length) . group . ( (\l -> dynamically_figure_out_sort_based_on_list_content l)) .  words . map toLower
</pre>

Or any of these combinations depending on situation.

Basically, we want the ability to compose functions (essence of functional programming) not by just type compatibility but through complex means that could be determined either at compile or at runtime while still maintaining the associativity of the composition.

For example, we want it to be impossible to write a function `detect` that can detect in what order the functions are composed:

```
detect ((f . g) . h) = True
detect (f . (g . h)) = False
```

(ref: https://www.reddit.com/r/haskell/comments/13vqlc/comment/c77n6cp/?utm_source=share&utm_medium=web2x&context=3)

Sure, what is the problem, then? Well with "." we are safe, it is universal operator operating on pure functions and not overloaded based on function type. All it matters that the input/output (types)  of functions being composed must match. We are able to compose any functions without knowing about them (their implementation). So "." is polymorphic.

But, we do not live in pure world, we need functions that do different things based on the value, not type: For example Maybe or Either or List (empty vs. non-empty case) and so on. Here, the composition is not based on type but kleisli <=< as input and outputs of the functions being composed do not match. And this operator is not polymorphic. Ask ChatGPT for details.


Still, what is the problem? Well, the composition of defined for the abstract data type T holding an object (like T in a -> T b) can define how the composition happens (via <=< operator that is being used to compose). For example see [this reference](https://stackoverflow.com/a/68075895). Also, different developers compose the same conditional logic differently (for example see monad_laws_assoc.hs and monad_laws_right_identity.hs).


So, we cannot expect the system to automatically do it, but rely on the developer to follow certain rules. What exactly are they (hint: monad laws) that we want the developer guarantee?

Enter monads...

( we need monadic associativity which can be defined in the form of equivalence of horizontal compositions 1 and mu or mu and 1). Note that the haskell Functor does not enforce associativity of functor composition, so just by requiring that Monad is a subclass of Functor is not enough. We (developer) still have to guarantee Monad laws. There is no Functor composition concept in HASK, we usually create our own Compose for it.  In math things are easy: if a construct does not follow monad laws, we just delcare that it is not monad. But in haskell, if an instance of Monad is created and bind is implemented for it (and not associative), we just can't automatically declare it as not being monadic. It still has implemented Monad typeclass contract (from the functions and their signature perspective).


Legend
------

<pre>

If F is a functor, F<sub>o</sub> maps an object from the source category to target category.
            and    F<sub>m</sub> maps an arrow from the source category to target category.

.   is composition in a category.

∘   is functor composition, sometime, just juxtaposition. This is also used for horizontal composition.

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

What we need is a (generic) function (generic function, works for all parameters, a natural transformation):
<pre>
mu: T<sub>o</sub><sup>2</sup> -> T<sub>o</sub>
or its instantiations (components or indexing by object):
mu<sub>x</sub>: T<sub>o</sub><sup>2</sup> x -> T<sub>o</sub> x
</pre>

So, the result is:
<pre>
gf = mu<sub>c</sub> . T<sub>m</sub> g . f :: a -> T<sub>o</sub> c
</pre>

Now let's say we created a new kleisli arrow:
<pre>
h : c -> T<sub>o</sub> d
</pre>

And we want to compose h with gf. We apply same idea of lifting.

<pre>
T<sub>m</sub> h :: T<sub>o</sub> c -> T<sub>o</sub> (T<sub>o</sub> d)
or
T<sub>m</sub> h :: T<sub>o</sub> c -> T<sub>o</sub><sup>2</sup> d
</pre>

So, the result is:
<pre>
hgf1 = T<sub>m</sub> h . gf  :  a -> T<sub>o</sub><sup>2</sup> d
hgf = mu<sub>d</sub> . T<sub>m</sub> h . gf
or
hgf = mu<sub>d</sub> . T<sub>m</sub> h . mu<sub>c</sub> . T<sub>m</sub> g . f :: a -> T<sub>o</sub> d
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
hg = mu<sub>d</sub> . T<sub>m</sub> h . g :: b -> T<sub>o</sub> d
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

Here bimap expects two functions which are natural transformmations. Because of above Identity functor a -> T<sub>o</sub> b can be treated as a natural transformation. <strong>TODO: replace this with unit in the analysis below to be super accurate.</strong>

Then redo above steps:

<pre>

f : a -> T<sub>o</sub> b
g : b -> T<sub>o</sub> c
h : c -> T<sub>o</sub> d


f : a -> T<sub>o</sub> b

T<sub>m</sub> g : T<sub>o</sub> b -> T<sub>o</sub> (T<sub>o</sub> c)

r1 = T<sub>m</sub> g . f : a -> T<sub>o</sub> (T<sub>o</sub> c)

r2 = mu<sub>c</sub> . r1 : a -> T1 c

Here T1 stands for T<sub>o</sub> resulted out of reduction of T<sub>o</sub> (T<sub>o</sub>) by mu<sub>c</sub>.

T<sub>m</sub> h : T1 c -> T1 (T<sub>o</sub> d)

r3 = T<sub>m</sub> h . r2 : a -> T1 (T<sub>o</sub> d)

lhs = mu<sub>d</sub> . r3 : a -> Tl d

Here Tl stands for reduction of T1 (T<sub>o</sub>) by mu<sub>d</sub>.
Hence Tl corresponds to reduction order (TxT)xT.

</pre>

Unrolling lhs and simplifying using above abbreviations:

<pre>
lhs = mu<sub>d</sub> . (T<sub>m</sub> h . (mu<sub>c</sub> . (T<sub>m</sub> g . f)))

lhs = h <> (g <> f)
</pre>

<strong>Hence h <> (g <> f) corrsponds to (TxT)xT.</strong>

Now try this way:

<pre>
g : b -> T<sub>o</sub> c
T<sub>m</sub> h : T<sub>o</sub> c -> T<sub>o</sub>(T<sub>o</sub> d)

r4 = T<sub>m</sub> h . g : b -> T<sub>o</sub> (T<sub>o</sub> d)

r5 = mu<sub>d</sub> . r4 : b -> T2 d

Here T2 corrsponds to T<sub>o</sub> resulted out of reduction of T<sub>o</sub> (T<sub>o</sub>) by mu<sub>d</sub>.

Now decide to compose with f:

T<sub>m</sub> r5 : T<sub>o</sub> b -> T<sub>o</sub> (T2 d)

r6 = T<sub>m</sub> r5 . f : a -> T<sub>o</sub> (T2 d)

rhs = mu<sub>d</sub> . r6  : T<sub>o</sub> -> Tr d

Here Tr stands for reduction of (T<sub>o</sub>) T2) by mu<sub>d</sub>.
Hence Tr corresponds to reduction order  Tx(TxT).
</pre>

Unrolling rhs and simplifying using above abbreviations:
<pre>
rhs = mu<sub>d</sub> . ( T<sub>m</sub> (mu<sub>d</sub> . (T<sub>m</sub> h . g) )  ) . f

rhs = mu<sub>d</sub> . T<sub>m</sub> (h <> g)  . f

rhs = (h <> g) <> f

</pre>

<strong>Hence (h <> g) <> f corrsponds to Tx(TxT).</strong>

We expect lhs = rhs

So, Tl = Tr

So (TxT)xT = Tx(TxT)

Composition must be associative.

Let's analyze this a little further:

<pre>

lhs = mu<sub>d</sub> . (T<sub>m</sub> h . (mu<sub>c</sub> . (T<sub>m</sub> g . f)))
rhs = mu<sub>d</sub> . T<sub>m</sub> (h <> g)  . f

OR

lhs = mu<sub>d</sub> . (T<sub>m</sub> h . (mu<sub>c</sub> . (T<sub>m</sub> g . f)))
rhs = mu<sub>d</sub> . ( T<sub>m</sub> (mu<sub>c</sub> . (T<sub>m</sub> h . g) )  ) . f

Naturality of mu:

  x      (T<sub>o</sub> (T<sub>o</sub> x)) ------ mu<sub>x</sub> --------> T<sub>o</sub> x
  |         |                             |
f |      (T<sub>o</sub> (T<sub>o</sub> f))                     T<sub>o</sub> f
  |         |                             |
  v         v                             v
 T<sub>o</sub> y   (T<sub>o</sub><sup>2</sup> (T<sub>o</sub> y)) ---- mu<sub>T<sub>o</sub> y</sub> -----> T<sub>o</sub> (T<sub>o</sub> y)
            .
            .
            .
            .  (associativity of functor composition)
            .
            .
            v
      T<sub>o</sub> (T<sub>o</sub><sup>2</sup> y)  ------ T<sub>m</sub> mu<sub>y</sub> ------> T<sub>o</sub> (T<sub>o</sub> y)        

lhs = mu<sub>d</sub> . (T<sub>m</sub> h . (mu<sub>c</sub> . (T<sub>m</sub> g . f)))
    = mu<sub>d</sub> . T<sub>m</sub> h . mu<sub>c</sub> . T<sub>m</sub> g . f
    = mu<sub>d</sub> . ( T<sub>m</sub> h . mu<sub>c</sub> ) . T<sub>m</sub> g . f
    = mu<sub>d</sub> . ( mu<sub>T<sub>o</sub> d</sub> . T<sub>m</sub><sup>2</sup> h ) . T<sub>m</sub> g . f    [ from natuality above with f = h, x = c, y = d]
    = mu<sub>d</sub> . mu<sub>T<sub>o</sub> d</sub> . T<sub>m</sub><sup>2</sup> h . T<sub>m</sub> g . f       
    = mu<sub>d</sub> . T<sub>m</sub> mu<sub>d</sub> . T<sub>m</sub> (T<sub>m</sub> h . g) . f        [ mu<sub>T<sub>o</sub> d</sub> = T<sub>m</sub> mu<sub>d</sub> i.e., associativity of functor composition;
                                                      and functor presevers composition ]
    = mu<sub>d</sub> . T<sub>m</sub> ( mu<sub>d</sub> . T<sub>m</sub> h . g) . f          [functor preserves composition]
    = rhs

(see also, https://math.stackexchange.com/questions/3392882/how-to-show-the-associativity-in-kleisli-category) 
So, to prove it we leveraged mu<sub>T<sub>o</sub> x</sub> = T<sub>m</sub> mu<sub>x</sub> which requires Tx(TxT) = (TxT)xT.

</pre>

You can see all of above pictorially here:

<img src="https://github.com/saibaba/monad_monoid/blob/master/naturality.png" width="600" />
<img src="https://github.com/saibaba/monad_monoid/blob/master/bifun_assoc.png" width="600" />
<img src="https://github.com/saibaba/monad_monoid/blob/master/monad_assoc.png" width="600" />

The red and green paths differ between T<sub>o</sub><sup>2</sup> c and T<sub>o</sub><sup>2</sup> d.

Naturality (blue path) allows us to prove that this is a commuting square.

But, our equations above use T<sub>m</sub> mu<sub>d</sub> for the bottom leg where as naturality 
uses mu<sub>T<sub>o</sub> d</sub>.

So, we have to add additional condition that mu<sub>T<sub>o</sub> d</sub> = T<sub>m</sub> mu<sub>d</sub> which is
achieved by endofunctor composition (bifunctor).


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

<strong>
So, the most important ingredients in understanding monads: endofunctor composition, join, and fmap with simplification by bifunctor. Note that you could have done everything without bifunctor too, it just simplifies things (like matrix in math).
</strong>

Notice that all in all, the associativity (and left/right unitor) of endofunctor composition directly translates to associativity of various operators like <=>, bind, or whatever else you define.

And, basically a monad gives ability to chain together or compose functions in using a more complex algorithm (each monad having its own algorithm) that are usually not composable just through type compatibility. See State monad for an example of elaborate algorithm.

Also, sometimes join is not just about removing one level of indirection introduced, but might have to do some computation. Again, it is a good idea to study State monad (implemented using join and fmap instead of bind) to understand this at a deeper level. See the 'state.hs' for this.

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

α<sub>b</sub> . F<sub>m</sub> f = G<sub>m</sub> f . α<sub>a</sub>

</pre>

Functor category [C, D] or Fun(C, D) or D<sup>C</sup>.
----------------------

<pre>
  Objects: functors like F: C-> D, G: C->D
  Morphisms: natural transformations, mu, eta, ...
  Composition: morphisms i.e., components of natural transformations compose as usual (vertical composition of natural transformations)
  Associativity: Composition is associative as it just normal function composition of morphisms(i.e., natural transformations).
  Identity: Identity natural transformation, I is the identity morphism.
</pre>


Endofunctor category [C,C]
--------------------------

It is basically, functor category from a category to the same, i.e., [C, C].



<pre>
  Objects: endofunctors like F, G above
  Morphisms: natural transformations, mu, eta, ...
  Composition: morphisms i.e., components of natural transformations compose as usual (vertical composition of natural transformations)
  Associativity: Composition is associative as it just normal function composition of morphisms(i.e., natural transformations).
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

Note that compiler validates this mechanically only at the type level. Programmer has to still make sure that associativity is true semantically in implementation.

Monoidal Category
-----------------

A monoidal category has an extra <strong>structure</strong> on top of a regular category. Additional structure is achieved through composite functors or category product and so on.

The way this is done is via a functor, called bifunctor.

Think of bifunctor taking two objects and returning another object all from the same category. For example it can take two objects and return a pair (assuming all pairs are also in the category). Another example is composite functor (one inside another like Maybe [] or [Maybe]). In both cases we have a functor from a domain which is a product of categories and range is another category. All being the same category in the discussion below.

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

Why is this called `monoid(al)`?
* Notice that a monoid is a structure with a binary operation (like addition/multiplication or something like above ⊗) from two objects of a set (like C X C above) to the same set (like C) above. 
* The operation is also associative.
* In monoidal category defined above, operation not strictly associate, but only upto a natural isomorphism by 3 natural (isomorphic) transformations:
** $\alpha_{A, B, C}$ = (A ⊗ B) ⊗ C -> A ⊗ (B ⊗ C). This is an arrow from functor to functor. Each is a functor because it is an arrow from (an object in a) category to  (an object in a) category. Both sides of arrow are not strictly equivalent, but upto natural isomorphism.
** $\lambda_{A}$ = 1 ⊗ A -> A. This is also an arrow from functor to functor (identity functor?).
** $\rho_{A}$ A ⊗ 1 -> A. 
** The 3 arrors also satisfy naturality condition:  Say we have an arrow f:$A_1\rightarrow A_2 in \mathcal{C}$. Then we can apply it either before or after $\lambda: \lambda_{A_2}\circ(1_\mathbf{1}\otimes f)=f\circ\lambda_{A_1}$ as arrows from $\mathbf{1}\otimes A_1 to A_2$.

See more on these rules here: https://unapologetic.wordpress.com/2007/06/28/monoidal-categories/

So, being a bifunctor, ⊗ operates on each $C_o x C_o$ (product) to produce an object in $C_o$. By definition, it is a functor as it maps both $C_o$ and $C_m$. So, each object, M ($\in C$) in a monoidal category satisfies:
* $\mu: M ⊗ M -> M$  (Notice it is same object M used as two arguments and resulting the same object again - a single object monoid)
* $\eta: I -> M$

(Here M can be something like Maybe with C = Endofunctors(HASK). Another example is List).


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
 
Note that above also means that the function resulted out of <strong>lifed functions (bimap)</strong> is nothing but the horizontal composition of natural transformations, α and β. For example,

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

A pure categorical definition of Monad is available in a sidebar.


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

Interesting thing to note about bind (>>=) is that a can be anything including another Counter.
And in this case, if function passed is id, that enclosed Counter can be simply extracted as it honors the `Counter b` restriction.
This is exactly join does.

<strong>join</strong> using <strong>bind</strong>:
<pre>
join:: m (m a) -> m a
join mma = bind mma id
</pre>

For more identities, see: https://en.wikipedia.org/wiki/Monad_(functional_programming)#fmap_and_join

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

bind f = join . fmap f

</pre>

Side Bar 4
----------
What is the difference between transformation and natural transformation?


<img src="https://github.com/saibaba/monad_monoid/blob/master/trans_vs_nat_trans.png" width="600" />

Side Bar 5
----------

Some notes on horizontal composition (a natural transformation from composite functor to another composite functor):

First note that natural transformations can be composed with each other. Since, however, transformations map objects to morphisms —instead of, e.g., objects to objects— they can not be simply applied one after another. Instead, composition is defined com- ponent wise, also called vertical composition.

And by defining a complex rule of composition instead of usual ".", natural transformations can be composed with each other, called horizontal composition.

<pre>
Horizontal composition is defined as:
  Let C, B, A be 3 categories.
  Functors S, T: C -> B
  Functors S', T': B -> A

  Let τ  : S  -> T
      τ' : S' -> T'
 
  τ' ∘ τ = T'<sub>m</sub> τ<sub>c</sub> . τ'<sub>S<sub>o</sub> c</sub>     Eqn-1
         = τ'<sub>T<sub>o</sub> c</sub> . S'<sub>m</sub> τ<sub>c</sub>     Eqn-2

  (note that this definition corresponds to bifunctor map, bimap defined elsewhere in this document!)

</pre>

The key point with horizontal transformation is that we are looking for a natural transformation from (composite) functor (S'.S) to (T'.T). And there are two actually: With $a \in C$, it can be (a) T'<sub>m</sub> τ<sub>a</sub>. τ'<sub>S<sub>o</sub>a</sub> or (b)  τ'<sub>T<sub>o</sub>a</sub> . S'<sub>m</sub> (τ<sub>a</sub>).  And fortunately due to naturality, they are equal.

Above horizontal composition is itself a natural transformation (see page 42 of Categories For the working mathematician for a proof).

Above can be restated using a combination of horizontal and vertical compositions. For example, above can be written as:

<pre>
  τ' ∘ τ = (T' ∘ τ) . (τ' ∘ S) 
         = (τ' ∘ T) . (S' ∘ τ)
</pre>

<pre>
Proof of τ' ∘ τ = (T' ∘ τ) . (τ' ∘ S) 

  T' τ<sub>c</sub> = Id<sub>T'<sub>o</sub>T<sub>o</sub> c</sub> . T' τ<sub>c</sub>
         = I<sup>T'</sup><sub>T<sub>o</sub>c </sub> . T' τ<sub>c</sub>
         = I<sup>T'</sup><sub>T<sub>o</sub>c </sub> ∘ τ<sub>c</sub>        (in comparison with Eqn-2)


  τ'<sub>S<sub>o</sub> c</sub> = τ'<sub>S<sub>o</sub> c</sub> . Id<sub>S'<sub>o</sub>S<sub>o</sub> c</sub>
         = τ'<sub>S<sub>o</sub> c</sub> . S'<sub>m</sub> (Id<sub>S<sub>o</sub> c</sub>)
         = τ'<sub>S<sub>o</sub> c</sub> . S'<sub>m</sub> I<sub>c</sub><sup>S</sup>
         = τ'<sub>S<sub>o</sub> c</sub> ∘ I<sub>c</sub><sup>S</sup>        (in comparison with Eqn-2)

  So,

  τ' ∘ τ = T'<sub>m</sub> τ<sub>c</sub> . τ'<sub>S<sub>o</sub> c</sub>     (from Eqn-1)
         = (I<sup>T'</sup><sub>T<sub>o</sub>c </sub> ∘ τ<sub>c</sub>) . (τ'<sub>S<sub>o</sub> c</sub> ∘ I<sub>c</sub><sup>S</sup>)

</pre>

Likewise,

<pre>
Proof of τ' ∘ τ = (τ' ∘ T) . (S' ∘ τ) 

  S' τ<sub>c</sub> = S' τ<sub>c</sub> . Id<sub>S'<sub>o</sub>S<sub>o</sub> c</sub>
         = S' τ<sub>c</sub> . I<sup>S'</sup><sub>S<sub>o</sub>c </sub>
         = I<sup>S'</sup><sub>S<sub>o</sub>c </sub> ∘ τ<sub>c</sub>        (in comparison with Eqn-1)


  τ'<sub>T<sub>o</sub> c</sub> = τ'<sub>T<sub>o</sub> c</sub> . Id<sub>S'<sub>o</sub>T<sub>o</sub> c</sub>
         = τ'<sub>T<sub>o</sub> c</sub> . S'<sub>m</sub> (Id<sub>T<sub>o</sub> c</sub>)
         = τ'<sub>T<sub>o</sub> c</sub> . S'<sub>m</sub> I<sub>c</sub><sup>T</sup>
         = τ'<sub>T<sub>o</sub> c</sub> ∘ I<sub>c</sub><sup>T</sup>        (in comparison with Eqn-2)

  So,

  τ' ∘ τ = τ'<sub>T<sub>o</sub> c</sub> . S'<sub>m</sub> τ<sub>c</sub>     (from Eqn-2)
         = (τ'<sub>T<sub>o</sub> c</sub> ∘ I<sub>c</sub><sup>T</sup>) . (I<sup>S'</sup><sub>S<sub>o</sub>c </sub> ∘ τ<sub>c</sub>)

</pre>

Then there is interchange law. See 'interchange.hs' to play with it.

See this: https://www.reddit.com/r/haskell/comments/5bwuh5/comment/d9s0b48/?utm_source=share&utm_medium=web2x&context=3

Side Bar 6
----------

Consider horizontal composition:

<img src="https://github.com/saibaba/monad_monoid/blob/master/horizcomp.png" width="600" />

Proof that ηE and Hη are both natural transformations is available in HaskellProgrammerGuideToIOMonad. Anyways, this is a special case of proofs in previous section about horizontal composition.

Now, specialize above diagram with one category and one functor only:

<img src="https://github.com/saibaba/monad_monoid/blob/master/horiz_specialize.png" width="600" />

Now define monad:

<img src="https://github.com/saibaba/monad_monoid/blob/master/monad.png" width="600" />

Here,

For an object, 'a', either select a natural component and then lift this morphism by T:

<pre>
μ<sub>a</sub> : T<sub>o</sub><sup>2</sup> a -> T a

T<sub>m</sub> μ<sub>a</sub> : T<sub>o</sub> (T<sub>o</sub><sup>2</sup> a) -> T<sub>o</sub> (T<sub>o</sub> a)
</pre>

Or lift the object 'a', and select a natural component of the lifted object:

<pre>
μ<sub>T<sub>o</sub> a</sub> : T<sub>o</sub><sup>2</sup> (T<sub>o</sub> a) -> T<sub>o</sub> (T<sub>o</sub> a)
</pre>


Side Bar 7
----------

More on transformation vs. natural transformation  (side bar 4):

Let C = { N, S, B }    where N = integers, S = strings, B = booleans
F = vector
G = Maybe

Let D = { vector[N], Maybe[N], vector[S], Maybe[S] ... }

Let T be place holder for an obect of C.

Let η = safeHead (::vector[T] -> Maybe[T] or F T -> G T).

For each object in C, we can index into η to get a morphism in D, η<sub>T</sub>. Hence η is a transformation.

Also, let's code it like this:

η<sub>N</sub> = { return Just 5 }
η<sub>S</sub> = { if input vector<string> is empty, return Nothing else, return Just <first element> }

Consider these 2 arbitrary morphisms in C:
f = inc  (in C, from N to N)
g = show (in C, from every element of C to S)

Consider two examples:

First:

safeHead<sub>N</sub> o fmap<sub>vector</sub> vs.  fmap<sub>Maybe</sub> f  o safeHead<sub>N</sub>


Second:

safeHead<sub>S</sub> o fmap<sub>vector</sub> vs.  fmap<sub>Maybe</sub> g o safeHead<sub>N</sub>


In both examples, the LHS vs. RHS do not match.

So, safeHead as defined above is not a natural transformation.

The core issue is that safeHead is able to peek into the type (N) and apply a different algorithm.

This is possible in C++ due to adhoc polymorphism (specialize a generic template function for a specific type like integer).


(Draw picture of categories involved for visualization)

An example in trans_not_natural2.cpp

Side Bar 8
----------

Elaboration for μ ∘ Tμ = μ ∘ μT:

First note that μ: T<sup>2</sup> -> T is a natural transformation.
And μ<sub>a</sub> is a morphism in C, so T can lift it (T can lift only morphisms not natural transformations)

So, we have a naturality square:

<pre>
                          T μ<sub>c</sub>
    T<sup>2</sup>(Tc) = T(T<sup>2</sup>c) ---------------->  T (Tc) = T<sup>2</sup>c
            |                            |
            |                            |
            | μ<sub>Tc</sub>                         | μ<sub>c</sub>
            |                            |
            |                            |
            v                            v
    T(Tc) = T<sup>2</sup>c ---------------->  T (Tc) = T<sup>2</sup>c
                          μ<sub>c</sub>
</pre>

Above, being a naturality square, commutes:

μ<sub>c</sub> ∘ Tμ<sub>c</sub> = μ<sub>c</sub> ∘  μ<sub>Tc</sub>

Observe:
* μ is a natural transformation
* Tμ is also a natural transformation. Why? It is a horizontal composition of 1<sub>T</sub> and μ.
* μ<sub>T</sub> is also a natural transformation. Why? It is a horizontal composition of μ and 1<sub>T</sub>.
* So, we can extract out c from above equation and remove it.
* Functor composition is associative, so T<sup>2</sup>(Tc) = T(T<sup>2</sup>c)

Using ~ for horiz. comp:

Bullet point 2:

1<sub>T</sub> ~ T μ = T μ

(The c component of μ is taken here as the input coming to it is c)

<pre>
   T<sup>2</sup>     T
  ---->   ------>
    |        |
    |        |
    μ       1<sub>T</sub>
    |        |
    v        v
  ---->   ------>
    T        T
</pre>


Bullet point 3:

(The Tc component of μ is taken here, as the input coming to it is Tc)

μ<sub>T</sub> ~ 1<sub>T</sub> = μ<sub>T</sub> = μT

<pre>
   T     T<sup>2</sup>
  ---->   ------>
    |        |
    |        |
  1<sub>T</sub>    μ<sub>T</sub>
    |        |
    v        v
  ---->   ------>
    T        T
</pre>

So, finally:
 
(μ ∘ Tμ)<sub>c</sub> = (μ ∘ μT)<sub>c</sub>
(μ ∘ Tμ) = (μ ∘ μT) (both sides are vertical composition of natural transformations, and hence natural transformations themselves).

Basically, this law means that for all a, T<sup>2</sup>T -> T<sup>2</sup> -> T or TT<sup>2</sup> -> T<sup>2</sup> -> T always lead to T when operated on by  Tμ or μT.

(https://math.stackexchange.com/questions/2101774/elaboration-for-%CE%BC-%E2%88%98t%CE%BC-%CE%BC-%E2%88%98-%CE%BCt-from-a-monad-definition)

Side Bar 9
---

Why Kleisli operator not polymorphic?

https://chatgpt.com/c/672c132b-5018-8010-81a8-05fe2a4f9cff

Consider

g:b→mc

f:a→mb

g <=< f :: a -> m c

But there are certain expectations on monad, m. It has to be the same (Maybe or List etc.,) in both f and g above. For any specific usage of <=<, the choice of m is fixed, making it monomorphic in the monad within any given instance of composition. They cannot be polymorphically same.

So, composing the following two is not possible with Kleisli operator:

f :: a -> Maybe b
g :: b -> [c]



Opposed to this, "." is polymorphic: 
(.) :: (b -> c) -> (a -> b) -> (a -> c)

Here there are no further constraints on a, b, c.

In summary, the Kleisli composition operator <=< is not polymorphic because it requires the two functions to work within the same monadic context, meaning the monad 𝑚.  m must be fixed for any given use of <=<. This is unlike truly polymorphic operators, such as ordinary function composition (.), which impose no such restriction and are hence fully polymorphic over types.
