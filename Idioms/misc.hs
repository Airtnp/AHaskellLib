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