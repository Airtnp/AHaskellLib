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

