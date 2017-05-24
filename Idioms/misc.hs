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

