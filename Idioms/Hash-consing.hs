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

wonderous2 :: Integer -> Integer
wonderous2 x
  | x <= maxMemo = memoArray ! x
  | otherwise    = wonderous2' x
  where
        maxMemo = 100
        memoArray = array (1,maxMemo)
                        [ (x, wonderous2' x) | x <- [1..maxMemo] ]
 
        wonderous2' 1 = 0
        wonderous2' x
          | even x    = 1 + wonderous2 (x `div` 2)
          | otherwise = 1 + wonderous2' (3*x+1)

-- When using this pattern in your own code, note carefully when to call the memoised version (wonderous2 in the above example) and when not to. In general, the partially memoised version (wonderous2' in the above example) should call the memoised version if it needs to perform a recursive call. However, in this instance, we only memoize for small values of x, so the branch of the recursion that passes a larger argument need not bother checking the memo table. (This does slow the array initialization, however.) Thanks to LazyEvaluation, we can even memoise an infinite domain, though we lose constant time lookup. This data structure is O(log N):

type MemoTable a = [(Integer, BinTree a)]
data BinTree a = Leaf a | Node Integer (BinTree a) (BinTree a)
 
wonderous3 :: Integer -> Integer
wonderous3 x
  = searchMemoTable x memoTable
  where
        memoTable :: MemoTable Integer
        memoTable = buildMemoTable 1 5
 
        buildMemoTable n i
            = (nextn, buildMemoTable' n i) : buildMemoTable nextn (i+1)
            where
                nextn = n + 2^i
 
                buildMemoTable' base 0
                    = Leaf (wonderous3' base)
                buildMemoTable' base i
                    = Node (base + midSize)
                           (buildMemoTable' base (i-1))
                           (buildMemoTable' (base + midSize) (i-1))
                    where
                        midSize = 2 ^ (i-1)
 
        searchMemoTable x ((x',tree):ms)
            | x < x'    = searchMemoTree x tree
            | otherwise = searchMemoTable x ms
 
        searchMemoTree x (Leaf y) = y
        searchMemoTree x (Node mid l r)
            | x < mid   = searchMemoTree x l
            | otherwise = searchMemoTree x r
 
        wonderous3' 1 = 0
        wonderous3' x
          | even x    = 1 + wonderous3 (x `div` 2)
          | otherwise = 1 + wonderous3 (3*x+1)

-- Naturally, these techniques can be combined, say, by using a fast CAF data structure for the most common part of the domain and an infinite CAF data structure for the rest.