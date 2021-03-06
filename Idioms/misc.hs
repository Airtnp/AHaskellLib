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

-- Mutually recursive modules

-- Mutually recursive modules are modules that import each other. This way it is not possible to find a sequence to compile them one after another. This is a typical problem of languages with a strong module system, in contrast to languages like C, where all parts of a program are merged textually by the preprocessor before compiling them.

module A where
 
import B
 
 
module B where
 
import A

-- If possible, mutually recursive modules should be avoided, since they complicate module dependencies. Once you have mutually recursive modules in a package, you will no longer be able to put modules of an import cycle into different packages, because mutually recursive packages are not supported.

-- GHC supports mutually recursive modules in a limited way and requires additional information. You must break the data dependency cycles manually by creating .hs-boot files. Up to version 6.10 it is not possible to create mutually recursive class definitions across modules, e.g.

module A where
 
import B
 
class B t => A t where
   ...
 
 
module B where
 
import A
 
class B t where
   f :: A t => t -> t

-- Not all mutual recursion can be solved by adding *.hs-boot files. For instance:

-- In the protocol-buffers package message definitions are used to generate Haskell source files. The messages *.hs files could be mutually recursive, and in easy cases the mutual recursion can be solved by adding *.hs-boot files and {- SOURCE -} pragmas. In difficult cases it is impossible to solve the recursion through adding *.hs-boot files, and in these cases the protocol-buffers package generates additional modules to break the difficult recursion cycles. Happily, the API of the message *.hs files does not change, and all the complexity is kept under the hood.

-- There are some ways to avoid mutually recursive imports, which we will describe below.

-- Use type parameters

-- If you have the definitions

module A where
 
import B
 
data A = A B
 
 
module B where
 
import A
 
data B = B A

-- then you can break the cycle by adding a type parameter to one of these data declarations. By thinking about that possibility you might find that you want to generalize one of the data structures anyway. This yields:

module A where
 
data A b = A b
 
 
module B where
 
import A
 
data B = B (A B)

-- This way you only generalize the data structure. All functions that use A may use it with the fixed type argument B.

-- Global type definitions

-- Some packages use to define all data types and classes in one module of the package. These types are then imported by all other modules of the package. This may however conflict with the use of qualified names, since in this style clashes of unqualified type identifiers in the type definition module are more likely.


-- Not just Maybe

-- Note: Many now consider the fail method of Monad to be a failure, as it defaults to throwing an exception. Modern Haskell style tends towards using total functions wherever possible, and this idiom has come increasingly to be frowned upon.

-- Non-trivial type synonyms

-- To avoid Miles/Km , feet/metres goofs:

{-

    Rather than

        type Miles = Int
    use

        newtype Miles = Miles Int
        
    together with

	toMiles :: Int -> Miles
	fromMiles :: Miles -> Int

    these can be redefined to be "id" later, after the code is stablized.

    No need to change things : The functions toMiles and fromMiles is already effectively id !

    A newtype definition just generates a new type whose implementation is the same as the old one.
    The constructor (and possibly selector) really does nothing except coerce between the new type and the old one.

    So as undefined is Haskell's notation for bottom
        toMiles undefined === undefined
    which isn't the case if we use a data definition.

    Structures might be a bit more convenient here.

    newtype Miles = Miles { fromMiles :: Int }
    toMiles = Miles
        {- or just use Miles, though this is harder to excise later -}
    For tracking more complex units, you may be able to get some of the way by using phantom types or creative use of functional dependencies.



-}


-- Obfuscation
-- ref: https://wiki.haskell.org/Obfuscation

-- IOHCC

-- Parallel machine

-- There are two reasonable ways to define an applicative functor instance for lists. Both of them simulate kinds of parallel computers:

    -- The plain instance simulates a non-deterministic computer. This instance can also be generalized to monads.

    -- The applicative functor instance of the ZipList wrapper simulates a SIMD (single-instruction multiple-data) computer.

-- Partial application

-- Partial application in Haskell involves passing less than the full number of arguments to a function that takes multiple arguments.

-- Partial signatures

-- "The regular (full) signature of a function specifies the type of the function and -- if the type includes constrained type variables -- enumerates all of the typeclass constraints. The list of the constraints may be quite large. Partial signatures help when:

    -- we wish to add an extra constraint to the type of the function but we do not wish to explicitly write the type of the function and enumerate all of the typeclass constraints,
    -- we wish to specify the type of the function and perhaps some of the constraints -- and let the typechecker figure out the rest of them.



-- Parallelism vs. Concurrency
-- ref: https://ghcmutterings.wordpress.com/2009/10/06/parallelism-concurrency/

    -- The term Parallelism refers to techniques to make programs faster by performing several computations in parallel. This requires hardware with multiple processing units. In many cases the sub-computations are of the same structure, but this is not necessary. Graphic computations on a GPU are parallelism. Key problem of parallelism is to reduce data dependencies in order to be able to perform computations on independent computation units with minimal communication between them. To this end it can be even an advantage to do the same computation twice on different units.

    -- The term Concurrency refers to techniques that make program more usable. Concurrency can be implemented and is used a lot on single processing units, nonetheless it may benefit from multiple processing units with respect to speed. If an operating system is called a multi-tasking operating system, this is a synonym for supporting concurrency. If you can load multiple documents simultaneously in the tabs of your browser and you can still open menus and perform more actions, this is concurrency.

    -- If you run distributed-net computations in the background while working with interactive applications in the foreground, that is concurrency. On the other hand dividing a task into packets that can be computed via distributed-net clients, this is parallelism.

    -- Let me tell an anecdote to further sharpen the difference: Amiga computers were always advertised for their multi-tasking operating system. However DOS/Windows-3.1 users were never attracted by this advertisement since they argued that a single CPU cannot be made faster by performing several tasks in an interleaved way. They were right, but this was not the point: Multitasking allows to avoid that the computer gets bored. Indeed in the eighties Amiga computers were considered great for raytracing. However the special graphics and sound hardware in Amiga computers could not help with raytracing. The important advantage was, that you could perform the graphics rendering concurrently to your daily work (office applications) without noticing the computation load of the raytracing. Multitasking just assigns the time between your keystrokes to the raytracer. However multitasking was not possible with most games, office software that eats all the memory or simply crashing applications. This leads to another confusing area: Error vs. Exception.

    -- If you need getNumCapabilities in your program, then your are certainly programming parallelism.
    -- If your parallelising efforts make sense on a single processor machine, too, then you are certainly programming concurrency.


-- Peano numbers

data Peano = Zero | Succ Peano

-- Separating shape and content
-- ref: https://mail.haskell.org/pipermail/haskell-cafe/2006-September/018204.html

{-

An interesting programming technique in Haskell is to encode a data structure as two separate structures, one defining the shape of the overall data, and the other defining the content.

This is used, for one, in nested data parallelism, to distribute nested arrays as flat arrays + a shape descriptor across a network.

-}

-- Top-level vs. local recursion

{-

Compare the following two implementations of map. The first one uses top-level recursion

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
whereas the second one uses local recursion

map :: (a -> b) -> [a] -> [b]
map f =
   let go []     = []
       go (x:xs) = f x : go xs
   in  go
Although the first version is shorter, there are reasons to prefer the second version for stylistic reasons:

It clearly shows that the 'f' is not "altered" in the recursion.
You cannot accidentally jump into the wrong loop. I often implement the same function more than once for comparison. The implementations may differ in laziness or performance. Their names are only slightly different and it happened too often that after copy&paste&name-change the recursive call went to the original (and thus wrong) function.
The local loop is probably also more efficient in execution, because the compiler does not need to move the f around. However, if this is actually more efficient then the compiler should do such a transformation itself.
Btw. the best implementation is probably foldr (\x acc -> f x : acc) []
which is both short and allows deforestation.

-}

-- Type variables instead of concrete types

{-

If you are new to Haskell you may think that type variables and polymorphism are fancy things that are rarely used. Maybe you are surprised that type variables and type class constraints can increase safety and readability also if you are eventually using only one concrete type.

Imagine the Prelude would contain the following functions:

maximum :: [Integer] -> Integer
sum :: [Integer] -> Integer
Sure, the names are carefully chosen and thus you can guess what they do. But the signature is not as expressive as it could be. Indeed these functions are in the Prelude but with a more general signature.

maximum :: (Ord a) => [a] -> a
sum :: (Num a) => [a] -> a
These functions can also be used for Integers,
but the signatures show which aspects of integers are actually required.

We realize that maximum is not about numbers, but can also be used for other ordered types, like Char. We can also conclude that maximum [] is undefined, since the Ord class has no function to construct certain values and the input list does not contain an element of the required type.

Now consider that you have a complex function that is hard to understand.

It is fixed to a concrete type, say Double.
You want to divide that function into a function that does the processing of the structure

and another function which does the calculations with Double.
This is good style in the sense of the "Separation of concerns" idiom. You do it, because you want to untangle the explicit recursion, which is hard to understand of its own, and the calculation, which also has pitfalls.

The structure processing does not know about the Double and it is wise to use type variables instead of Double Making the example more concrete, look at a state monad transformer StateT
which shall be nested by a nesting depth that is only known at runtime.

Ok that's not possible, so just consider a State applicative functor, that shall be nested the same way.
The functor depends on an input of the same type as its functor output,

that is we nest functions of type (Double -> State Double Double).
The nested functor has a list of state values as state value. The nesting depth depends on the length of the list of state values. (This design also forbids transformer techniques for general applicative functors.)

We could write a nesting function fixed to type Double
stackStates :: (Double -> State Double Double) -> (Double -> State [Double] Double)
but it is too easy to mix up state and return value here, because they have the same type. You should really separate that

stackStates :: (a -> State s a) -> (a -> State [s] a)
stackStates m  =  State . List.mapAccumL (runState . m)
also if you only use it for Double.
This way the type checker asserts, that you never mix up the state with the other type.



-}