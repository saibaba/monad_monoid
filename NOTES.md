Introduction
============

Experiments to understand: "A monad is just a monoid in the category of endofunctors, what's the probleⅿ?"

Most of the stuff is just re-iteration of what is said and learnt from the refrences given.

* Functor Composition
  * mapping objects
  * mapping morphisms

* Moniods
  * binary operation
  * identity law
  * associativity law

* Monad as monoid
  * binary operation
  * identity law
  * associativity law
  * return vs. id and bind vs. join

Two things to keep in mind:
  - A functor is a way of operating on the content of something without touching the structure.
  - A natural transformation is a way of operating on the structure of something without touching or looking at the content.

The dirty details are spelled out in https://github.com/saibaba/monad_monoid/blob/master/README.md.

Free theorem:

"Haskell's parametric polymorphism has an unexpected consequence: any polymorphic function of the type:

alpha :: F a -> G a
"where F and G are functors, automatically satisfies the naturality condition."

This is true, but assumes that the F and G themselves honor (by the developer) functor laws:

The requirement of compatibility with the actions of the functors is not expressible as a type signature, so we require it as a law:
(https://blog.merovius.de/posts/2018-01-08-monads-are-just-monoids/#fn:6)

f :: a->b

alpha_b . fmap_F f = fmap_G f . alpha_a

Ref
==

* Nice example of natural transformation and an example where it does not apply: https://stackoverflow.com/a/58364169 (another one comes to mind, you have a stream of Employee objects and stream is in the order of tenure- you built a generic function to handle the stream, generic in the sense does not know it is holding Employee objects - your boss suddenly tells to increase salary by 10% for first x employees, the generic function now does not work as it does not know that it is holding Employee objects, you have to make it to type specific to access/update salary field of Employee - you break natural transformation)
* Great introduction to categories: http://people.math.harvard.edu/~mazur/preprints/when_is_one.pdf
* Comprehending Monads - P. Wadler
* https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-proble%E2%85%BF
* http://w.pitula.me/2016/monad-proof/
* https://medium.com/@sinisalouc/demistifying-the-monad-in-scala-part-2-a-category-theory-approach-2f0a6d370eff
* https://stackoverflow.com/questions/19774564/what-does-it-mean-to-compose-two-functors
* http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html
* https://dzone.com/articles/functor-and-monad-examples-in-plain-java
* https://www.quora.com/Is-a-monad-really-a-monoid-in-the-category-of-endofunctors
* https://unapologetic.wordpress.com/2007/06/28/monoidal-categories/
* https://stackoverflow.com/questions/41073862/what-are-bifunctors-used-for-that-cant-be-achieved-by-composing-functors
* http://www.stephendiehl.com/posts/monads.html
* http://newartisans.com/2017/05/monads-are-monoids/
* https://books.google.com/books?id=MXboNPdTv7QC&pg=PA138&lpg=PA138&dq=%22monoid+in+the+category+of+endofunctors%22+mac+lane&source=bl&ots=feQWTkH2Uw&sig=tv-1JwaMOygKGmFE2vM2FhJVS9o&hl=en&ei=5iWsTJCkBIPSsAPQwJ36Aw&sa=X&oi=book_result&ct=result&resnum=1&ved=0CBIQ6AEwAA#v=onepage&q&f=false
* Uses of kleisli category (point-free monadic composition) : https://www.quora.com/In-Haskell-programming-language-what-are-some-practical-uses-of-Kleisli-composition
* http://w.pitula.me/2016/monad-proof/
* VI_stamatova_monoidal_cats.pdf
* https://proofwiki.org/wiki/Definition:Composition_of_Functors
* https://wiki.haskell.org/Monad_Laws
* http://etymon.blogspot.com/2006/09/monad-laws.html
* http://lambda-the-ultimate.org/node/2448
* https://math.stackexchange.com/questions/523906/show-that-function-compositions-are-associative
* https://www.quora.com/What-is-the-purpose-of-identity-morphisms-in-category-theory
* https://en.wikipedia.org/wiki/Monad_(functional_programming)
* http://members.chello.nl/hjgtuyl/tourdemonad.html (various monad related apis in Haskell  and how to use them with examples)
* The Haskell Programmer's Guide to the IO Monad (Don't Panic) has wealth of information : horizontal composition; transformation vs. naturality condition; kleisli star = point-free version of bind; 
* Category Theory For the Sciences, Theorem 5.3.2.20 (Interchange)
* https://arxiv.org/pdf/1405.3073.pdf (Categories from scatch - references seem to be good)
* Nice explanation of functor in category thy vs prog lang: https://cs.stackexchange.com/questions/9769/what-is-the-relation-between-functors-in-sml-and-category-theory
* Great learning resource on category theory: http://katmat.math.uni-bremen.de/acc/acc.pdf
* https://www.quora.com/What-is-the-difference-between-type-constructor-and-a-functor-in-Haskell (nice stuff on contra/covariant)
* Naturality follows from parametricity: https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/
* Function is a functor: https://eli.thegreenplace.net/2018/haskell-functions-as-functors-applicatives-and-monads/#id7
* An example of transformation that is not natural: https://math.stackexchange.com/questions/2772832/non-natural-transformation-example. A sample code is in trans_not_natural.cpp.
* An extended example of transformation that is not natural: trans_not_natural2.cpp.
* Nice notes on natural transformations: https://pages.cs.wisc.edu/~jcyphert/categoryTheoryNotes/basics/3_NaturalTransformations.pdf
* https://gist.github.com/programaker/7f36e6c454d0894a9368f99ff4208eeb
* What is one use of Kleisli composition? In monadic programming it is equivalent of point-free style: https://www.quora.com/In-Haskell-programming-language-what-are-some-practical-uses-of-Kleisli-composition
* Seeing horizontal composition simply as a bifunctor: https://www.reddit.com/r/haskell/comments/5bwuh5/comment/d9s0b48/?utm_source=share&utm_medium=web2x&context=3 (taking 2 natural transformations in the input, returning horizontal composition as output, which is nothing but the natural transformation from composite functor to another composite functor). Study it in conjunction with: https://bartoszmilewski.com/2016/12/27/monads-categorically/
* https://miklos-martin.github.io/learn/fp/2016/03/10/monad-laws-for-regular-developers.html
* https://evinsellin.medium.com/teaching-monads-slightly-differently-2af62c4af8ce
* https://www.cs.cmu.edu/~crary/819-f09/Moggi91.pdf
* Monad laws in haiku: "Kleisli composition forms a category": https://wiki.haskell.org/Monad_laws
* https://stackoverflow.com/questions/34398239/with-monads-can-join-be-defined-in-terms-of-bind
* https://math.stackexchange.com/questions/2101774/elaboration-for-%CE%BC-%E2%88%98t%CE%BC-%CE%BC-%E2%88%98-%CE%BCt-from-a-monad-definition
* https://stackoverflow.com/questions/68070620/is-it-possible-for-a-non-io-monad-to-violate-associativity-in-haskell
* http://ku-fpg.github.io/files/Sculthorpe-13-ConstrainedMonad.pdf
* https://jeltsch.wordpress.com/2013/02/14/the-constraint-kind/
* https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
* https://github.com/quchen/articles/blob/master/second_functor_law.md
* Question about monad associativity law: http://lambda-the-ultimate.org/node/2448
* https://stackoverflow.com/questions/3322540/how-and-why-does-the-haskell-cont-monad-work
* See https://en.wikipedia.org/wiki/Monad_(functional_programming)#Usage for good comparison with other common constructs used by programmers all the time.
* Viewing monad as overridable semicolon: https://stackoverflow.com/a/12264535
* Lots of good references in the lecture notes here: https://www.csc.kth.se/utbildning/kth/kurser/2D1456/avfunk07/view.php?arg=lectures.h&m=1
* Something very interesting going on with propsition on page 7 of https://math.uchicago.edu/~may/VIGRE/VIGRE2007/REUPapers/FINALAPP/Jerzak.pdf (equivalence between product category and natural transformation). It is saying you can write alpha (for example safehead) without knowing anything the objects of C (for example HASK).
* https://cs.stackexchange.com/questions/119777/functor-laws-and-natural-transformations-in-haskell
* https://stackoverflow.com/a/48664770 (added benefit a monad over functor: functor lifts a->b but monad operates on a->m b and hence allows you to return a monadic value, for example an error condition.)
* Explainn monad by handcrafting list: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/13-the-list-monad
* https://wiki.haskell.org/User:Michiexile/MATH198/Lecture_7 (leading from categorical product to non cartesian product to functor composition): a monad in a category C to be a monoid object in the category of endofunctors on that category. This directly traslnates to A monad in a category C is an endofunctor T: C\to C equipped with natural transformations \mu: T^2\to T and \eta: 1\to T ...
* https://groups.seas.harvard.edu/courses/cs152/2021sp/lectures/lec18-monads.pdf
* mappend is more general than (++) for list: https://stackoverflow.com/questions/10961483/haskell-duplicated-functions-and-mappend. For a monad, the monoid is the composite endofunctor (m m), and mappend is for mu? From wikipedia, any monad both gives rise to a category (called the Kleisli category) and a monoid in the category of functors (from values to computations), with monadic composition as a binary operator in the monoid and unit as identity in the monad.
* If you get a hidden package error when loading a module, install it with --lib option, for example for QuickCheck `cabal install QuickCheck --lib`.
* See important notes in `Further Reading` section: https://wiki.haskell.org/Typeclassopedia#Monoid
* Apples/blueberry/functor: If you will give me a blueberry for each apple I give you (a -> b), and I have a box of apples (f a), then I can get a box of blueberries (f b): https://wiki.haskell.org/Monads_as_containers)
* See this free monad answer: https://stackoverflow.com/a/13357359. Particularly notice how, since an endofunctor (or monad) can hold an instance of itself (endofunctor composition) -- i.e., its monoidic properties are leveraged directly to create a placeholder monad. Note that if a monad does not use any special properties of its structure, it is free. For example Maybe is free monad. List is not as it uses mappend in its bind/join.
* Best intro to Free Monads: https://deque.blog/2017/11/13/free-monads-from-basics-up-to-implementing-composable-and-effectful-stream-processing/
