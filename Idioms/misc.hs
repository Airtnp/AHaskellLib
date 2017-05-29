-- Misc

-- Almost Haskell
-- ref: https://wiki.haskell.org/Almost_Haskell

-- Perfunctors are like functors, but more perfunctory. We have:
prefunctory :: (Perfunctor (f :: * -> *)) => f a -> f a

-- Base cases and identities
-- ref: https://wiki.haskell.org/Base_cases_and_identities

-- To determine base cases
sum [] = {- How to determine? -} 0
sum (x:xs) = x + sum xs

{-
    Pred:
        sum [x] == x
            sum xs + sum ys == sum (xs ++ ys)

    Deduct: 
        sum [] + sum [0] == sum ([] ++ [0])
            => sum [] + 0 == 0
            => sum [] == 0
-}

-- Blueprint
-- ref: https://wiki.haskell.org/Blueprint
-- cafe: https://mail.haskell.org/pipermail/haskell-cafe/2006-September/018133.html

-- The goal of the blueprint technique is to allow reading a data structure like Data.Map while constructing it.
-- The idea is to separate the structure from the contained data.


-- Combinator pattern
-- Libraries such as Parsec use the combinator pattern, where complex structures are built by defining a small set of very simple 'primitives', and a set of 'combinators' for combining them into more complicated structures. It's somewhat similar to the Composition pattern found in object-oriented programming.

{-
    In the case of the Parsec, the library provides a set of extremely simple (almost trivial) parsers, and ways to combine small parsers into bigger parsers. Many other libraries and programs use the same ideas to build other structures:

        Parsec builds parsers out of smaller parsers.

        The School of Expression (SOE) graphics library builds pictures out of individual shapes.
        
        The SOE book also mentions a library to build music out of individual notes and rests.

        Another textbook describes building financial contracts.

        [Software transactional memory] builds big transactions out of smaller ones.

        The Haskell IO system itself builds whole programs out of small I/O actions using >>= and return.
-}



-- Composing functions with multiple values
-- (++) :: [a] -> [a] -> [a]
-- How to return [a] -> [a] from ([a], [a])?

-- just uncurry it
-- uncurry (++) :: ([a], [a]) -> [a]


-- Default values in records
{-
    You cannot associate default values with a type, but you can define as many record values as you like and use them as basis for updating elements.

    data Foo = Foo { bar :: Int, baz :: Int, quux :: Int }
    
    fooDefault = Foo { bar = 1, baz = 2, quux = 3 }
    
    newRecord = fooDefault { quux = 42 }

    If you only want some of the fields to be defaulted, you can make them undefined in the default record. Unfortunately this won't be statically checked by the compiler.
-}

-- High Order Function

-- A higher-order function is a function that takes other functions as arguments or returns a function as result.
-- The major use is to abstract common behaviour into one place.


-- Indent
-- ref: https://wiki.haskell.org/Indent

-- Emacs/GHC/ghc-api/Language.Haskell/Lambdabot
-- --ddump-parsed
-- ghc -O a.hs -package haskell-src


-- Inferring types
-- ref: https://wiki.haskell.org/Inferring_types

-- Have the compiler infer types for you automatically
-- GHCi/Editor support/Compiler support


{-

   #!/bin/sh
   # input is a top level .hs decls
   FILE=$*
   DECL=`cat`
   ID=`echo $DECL | sed 's/^\([^ ]*\).*/\1/'`
   echo ":t $ID" | ghci -v0 -cpp -fglasgow-exts -w $FILE
   echo $DECL

    :map ty :.!typeOf %

-}

-- $ ghc -ddump-tc A.hs 2>&1 | sed '/^\=/d;/AbsBinds/d;/ *\[\]$/d'


-- Infix expressions

--  xs `zipWith (+)` ys

infixr 0 -:, :-
data Infix f y = f :- y
-- x -:f:- y = x `f` y
-- main = print $ [1,2,3] -: zipWith (+) :- [4,5,6]

-- For completeness, here's the `dual':

infixl 5 -!
(-!) = flip ($)
infixl 5 !-
(!-) = ($)
 
add2 x y = x + y
add3 x y z = x + y + z
add4 x y z u = x + y + z + u
sub3 x y z = x + y - z
 
testa1 = 1 -! add2 !- 3 + 4
testa2 = 1 -! add3 1 !- 3 + 4
testa3 = 1 - 2 -! add4 1  5 !- 3 * 4
-- 17 = (1-2) + (1+5) + (3*4) 
testa4 = 1 - 2 -! sub3 1  !- 3 * 4 
-- -12 = (1-2) + (1) - 12

-- Introduction to IO
-- ref: https://wiki.haskell.org/Introduction_to_IO



-- Lens
-- ref: https://wiki.haskell.org/Lens
-- see hello_world/lens


-- Monadic Region
-- ref: http://okmij.org/ftp/Haskell/regions.html

{-

Monadic Regions is a technique for managing resources such as memory areas, file handles, database connections, etc. It was introduced by Tofte and Talpin as a memory allocation technique, and is implemented in ML-Kit and Cyclone. A region is an area of memory holding heap allocated data (reference cells). Regions may nest and so more than one region may be active at any given point. A new reference cell may only be allocated in an active region, and may then only be used while that region is active. The system statically guarantees that no cell can be accessed when its region is closed. Therefore, all cells in a region can be immediately garbage collected when the region is closed.
Regions offer an attractive alternative to both manual allocation of resources and garbage collection. Unlike the latter, region-based resource management makes resource disposal and finalization predictable. We can precisely identify the program points where allocated resources are freed and finalization actions are run. Like other automatic resource managements schemes, regions statically assure us that no resource is used after it is disposed of, no resource is freed twice, and all resources are eventually deallocated.

Monadic Regions can manage resources other than memory, as explained by Brandon Moore: ``I'm assuming you understand how the type on runST and the STRef operations ensure that, even though you can smuggle out an STRef in the result from runST, you will never be able to use it again. The idea was to do the equivalent thing with databases: use fancy types to ensure that handle can only be used inside to origination withDB or withCursor or whatever, and the bracketing function can release the resource on the way out, without worrying about it being used again.'' In a follow-up, Benjamin Franksen concurred: ``I think this is an extremely good idea. I have been very frustrated with finalizers because of their limitations (can't rely on them being called at all), so have (reluctantly) been using the unsafe bracket version. Making it safe via a type system trick is really the way to go.''

The paper by Fluet and Morrisett describes monadic regions, their calculus, and the type- and meaning-preserving translation to a variant of System F. The authors mention as future work providing a RGN monad for Haskell programmers. We show exactly this monad in Section Heavy-weight implementation of region calculus , which however relies on heavy type-class trickery. A simpler, and still complete, solution exists, especially suitable for IO regions and ensuring the validity of file handles: Lightweight monadic regions .

With some limitations, File and Database IO regions can be implemented already in Haskell98 with a common extension for rank-2 types (existentials). This technique has indeed been used in Takusen, to provide precisely the assurances that Brandon Moore wanted. One may hope this assured approach to File IO would be more widely used.


-}

-- Multiple instances

-- I like to define multiple type class instances for the same pair of class and type. Sometimes I also need two instances where the order of type parameters is different.

-- E.g. I want to define two Functor instances for a pair: One instance where the first member is mapped and another instance where the second member is mapped. How is it possible?

{-

    You can define multiple type class instances for the same pair of class and type if you keep the class and type definitions in different modules, i.e. not the modules than contains the instance declarations. These instances are therefore called orphan instances.

    However this isn't perfect, since you must ensure that two modules with conflicting instances declarations are never imported together because instance declarations are automatically imported and cannot be hidden.

    Furthermore, modules which import conflicting modules only indirectly conflict itself.

    Thus multiple instances should be avoided, and a safe way to do this is to avoid orphan instances. You can achieve this by wrapping the type in a newtype and lift all required instances to that new type.
    If you do not fear language extensions you can simplify this task considerably using the GeneralizedNewtypeDeriving feature. The custom instance can be defined for the class/newtype pair and it is not orphan if it is defined where newtype is introduced.

    Using newtype you can also change the order of type parameters or give type parameters a fixed type.
    Example: There are so many types and operations that exhibit a monoid structure,

    but it would not be useful to call all the operations mempty and mappend. It is however useful to call them via the Monoid interface sometimes, e.g. in the Writer monad. Thus the module Data.Monoid provides several newtype wrappers for common monoids.

-}

-- Orphan instance
-- ref: https://mail.haskell.org/pipermail/libraries/2008-August/010399.html

{-

    An orphan instance is a type class instance for class C and type T which is neither defined in the module where C is defined nor in the module where T is defined.

    Type class instances are special in that they don't have a name and cannot be imported explicitly. This also means that they cannot be excluded explicitly. All instances defined in a module A are imported automatically when importing A, or importing any module that imports A, directly or indirectly.

    Say you want to define an alternative instance to an existing instance. This is a bad thing, since if two instances for the same class/type pair are in scope, then you cannot describe in Haskell 98 which instance to use. If you want to use multiple instances for the same class/type, you have to ensure that they are never imported together in a module somewhere. It is almost impossible to assert that, or put differently, it would reduce the composability of libraries considerably.

    Actually, non-orphan instances can avoid definition of multiple instances. For defining an instance you have to import the class and the type and then you will automatically have the according non-orphan instances imported, too. If you want to define a new instance then the compiler will reject it immediately.

    2 Common workaround
    Let's say you want an instance for Monoid Int that handles addition. Later on, you'd like to reuse that code for handling multiplication. But now, there's two definitions of Monoid Int!

    A common workaround is to wrap your type in a newtype and create an instance for that newtype. Then, marshal (=convert) between the original type and the newtype where necessary. In our example, we would have two newtypes: one for addition and one for multiplication. We would then create Monoid instances for each newtype wrapper.

    3 When Orphan Instances can be useful
    It is worth noting that Orphan Instances can be viewed as a mechanism for writing modules of code with a fixed typed interface, but parameterized over the choice of implementation. In this case, Orphan Instances act as a sort of plugin architecture for providing alternative implementations with a uniform interface.

    A basic treatment of the relationship between type classes and modules (in the SML sense of modules) can be found at http://www.mpi-sws.org/~dreyer/papers/mtc/main-short.pdf and http://www.cse.unsw.edu.au/~chak/papers/modules-classes.pdf

-}