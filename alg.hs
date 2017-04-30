module Alg where

import Data.List

-- For intentive solution sum = sum - xs[i-m] + xs[i]
min_sub_list :: (Num a, Ord a) => [a] -> Int -> a
min_sub_list xs m = init_sum + min_diff
    where
        shift = drop m xs
        init_sum = sum $ take m xs
        min_diff = minimum $ scanl (+) 0 $ zipWith (-) shift xs