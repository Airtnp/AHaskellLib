-- Data structures not functions
-- ref: https://wiki.haskell.org/Data_structures_not_functions

-- Functions not data structures
-- ref: https://wiki.haskell.org/Functions_not_data_structures

-- Data structures not functions

-- Sometimes the best way to implement a function is as a data structure.

-- A good way to implement this is as an infinite stream of results, which is passed to an "interpreter" which selects which result is the most appropriate.

-- instead of this loop

squareRoot n = squareRoot' n (initialGuess n)
    where
        squareRoot' x
            | closeEnough x = x
            | otherwise     = squareRoot' (0.5 * (x + n/x))

-- better write

squareRoot n = head . filter closeEnough $ squareRoot' n (initialGuess n)
 
squareRoot' n x = iterate (\x -> 0.5 * (x + n/x)) x

-- This is easier to test and debug, because the intermediate form doubles as an execution trace.
-- There is a clean separation of responsibilities. If someone wants to use the algorithm with a different "close enough" test, or a better initial guess, it's easy to do.

-- if you implement this using standard list functions, such as head, filter and iterate, there may be no performance penalty. If the compiler uses a modern deforestation optimization, such as stream fusion, the intermediate data structure will be compiled away.

-- Stream fusion
-- Prototyping monads


-- Functions not data structures

-- represent data is using a function

-- Church encoding can be used to represent any algebraic data type using only functions.

-- FiniteMap

type FiniteMap key elt = key -> Maybe elt
 
emptyFM :: FiniteMap key elt
emptyFM = \k' -> Nothing
 
addToFM :: (Eq key) => FiniteMap key elt -> key -> elt -> FiniteMap key elt
addToFM m k v = \k' -> if (k == k') then Just v else m k'
 
delFromFM :: (Eq k) => FiniteMap key elt -> key -> FiniteMap key elt
delFromFM  m k = \k' -> if (k == k') then Nothing else m k'
 
lookupFM :: (Eq k) => FiniteMap key elt -> key -> Maybe elt
lookupFM m k = m k