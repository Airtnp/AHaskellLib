-- Extensions

-- Implicit parameters

{-# LANGUAGE ImplicitParams #-}

import Data.List (sortBy)

sortBy' :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sortBy' = sortBy ?cmp
sort :: Ord a => [a] -> [a]
sort = let ?cmp = compare in sortBy'

main = putStrLn (show (sort [3,1,2]))