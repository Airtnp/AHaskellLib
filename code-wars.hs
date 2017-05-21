module CodeWars where

import Data.Char
import Data.List
import Data.String
import Text.Printf
import Data.Bits

-- (\\) --> remove ys in xs
isIsogram :: String -> Bool
isIsogram s = null $ map toLower s \\ ['a'..'z']

-- elems :: Set a -> [a]
-- O(n). Convert the set to an ascending list of elements. Subject to list fusion.

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- === f <$> arg1 <*> arg2

-- cannot use in 0.00 or something
truncateDouble :: Double -> Integer -> Double
truncateDouble f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- Text.Printf.printf
-- a -> String --> show/gshow

-- For operation on determined array
-- use lazy-evaluation
-- arr = map f [1, 2, ..]
-- result = take n/takeWhile

-- Int -> String --> show
-- Int -> Char --> chr (n+48) | intToDigit
-- Char -> Int --> digitToInt

-- elemIndices  find indices of element

-- words <-> unwords === intercalate " " [words] === concat (intersperse [" "] [words])

-- non-exhaust pattern : cannot match

-- Test.Hspec
-- Test.QuickCheck

countL :: String -> Strint -> Int
countL str s = length $ filter (\i -> elem i s) str

-- with context

-- concatMap : map and concat
subSequence_alt :: [a] -> [[a]]
subSequence_alt = filter (not . null) . concatMap (map snd . splits . fst) . splits

-- 18 questions in collection-list
