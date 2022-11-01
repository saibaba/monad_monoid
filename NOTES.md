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
