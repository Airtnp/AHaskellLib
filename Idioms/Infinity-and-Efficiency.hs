-- Infinite and Efficiency
-- ref: https://wiki.haskell.org/Infinity_and_efficiency

-- How to check the efficiency of an implementation by checking for proper results for infinite input.  In general, it is harder to reason about time and memory complexity of an implementation than about its correctness. In fact, in Haskell inefficient implementations sometimes turn out to be wrong implementations.

-- Not right for infinite lists/inefficient

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p = reverse . dropWhile p . reverse

-- better

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p =
   foldr (\x xs -> if p x && null xs then [] else x:xs) []

-- Here, foldr formally inspects the list from right to left, but it actually processes data from left to right. Whenever a run of elements that matches the condition p occurs, these elements are held until the end of the list is encountered (then they are dropped), or when a non-matching list element is found (then they are emitted). The crux is the part null xs, which requires to do recursive calls within foldr. This works in many cases, but it fails if the number of matching elements becomes too large. The maximum memory consumption depends on the length of the runs of non-matching elements, which is much more efficient than the naive implementation above.