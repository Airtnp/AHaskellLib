-- Hash consing
-- ref: https://wiki.haskell.org/MemoisingCafs
-- ref: https://wiki.haskell.org/Tying_the_Knot

-- Use a table of already-built structures to increase sharing.

-- 1. Memoising CAFs

-- Memoising constructor functions gives you HashConsing, and you can sometimes use MemoisingCafs to implement that.

-- The MemoisingCafs idiom also supports recursion.

-- 3x+1 problem
wonderous :: Integer -> Integer
wonderous 1 = 0
wonderous x
  | even x    = 1 + wonderous (x `div` 2)
  | otherwise = 1 + wonderous (3*x+1)

-- we can memoise some of the domain using an array CAF