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