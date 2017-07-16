module Alg where

import Data.List
import Control.Monad
import Control.Monad.Instances

-- For intentive solution sum = sum - xs[i-m] + xs[i]
min_sub_list :: (Num a, Ord a) => [a] -> Int -> a
min_sub_list xs m = init_sum + min_diff
    where
        shift = drop m xs
        init_sum = sum $ take m xs
        min_diff = minimum $ scanl (+) 0 $ zipWith (-) shift xs

-- In Haskell, better use merge. qsort has O(nlgn) space complexity
-- ref: https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.OldList.html#sort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<x) xs) ++ [x] ++ qsort (filter (>=x) xs)


-- simple O(n) Euler sieve
primes = sieve [2..]
sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

isPrime2 :: Integer -> Bool
isPrime2 n
  | n < 0 = isPrime (-n)
  | n == 0 = False
  | n == 1 = False
  | n == 2 = True
  | otherwise = and $ map (jb . (mod n)) [2..truncate(sqrt(fromInteger n))]
  where jb 0 = False
        jb _ = True

-- square = join (*)
is_prime = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes_alt
-- is_prime = ap (all . ((0 /=) . ) . mod) $ flip takeWhile primes_alt . (. (^2)) . (>=)
primes_alt = 2 : filter is_prime [3, 5..]

fib = 1 : 1 : zipWith (+) fib (tail fib)

-- 8-queen problem
queen_safe :: Int -> [Int] -> Int -> Bool
queen_safe _ [] _ = True
queen_safe x (x1:xs) n = 
    x /= x1
    && x /= x1 + n && x /= x1 - n
    && queen_safe x xs (n+1)

queen_num :: Int -> [[Int]]
queen_num 0 = [[]]
queen_num n = [ x : y | y <- queen_num (n-1), x <- [1..8], queen_safe x y 1]

-- TODO: Knuth-Morris-Partt substring
-- ref: https://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell