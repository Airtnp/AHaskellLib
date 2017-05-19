-- Difference lists
-- ref: https://wiki.haskell.org/Difference_list
-- wiki: https://en.wikipedia.org/wiki/Difference_list
-- stackoverflow: http://stackoverflow.com/questions/9197913/what-is-the-shows-trick-in-haskell

-- 1. As functions

-- A difference list of the second sort represents lists as a function f, which when given a list x, returns the list that f represents, prepended to x.

-- Whether this kind of difference list is more efficient than another list representations depends on usage patterns. If an algorithm builds a list by concatenating smaller lists, which are themselves built by concatenating still smaller lists, then use of difference lists can improve performance by effectively "flattening" the list building computations.

-- This can best be exemplified by show and shows of Prelude, where the first one implements the naive approach and the second one uses difference lists.

-- Binary tree

-- L-T-R

    -- (show L) ++ (show T ++ (show R))
    
    -- ((show LL) ++ (show LT ++ (show LR))) ++ (show T ++ (show R))
    
    -- (((show LLL) ++ (show LLT ++ (show LLR))) ++ (show LT ++ (show LR))) ++ (show T ++ (show R))

-- If the tree is large, you end up with a pretty large left association for the left subtree. True, there's lot of right association, too, but bad enough.

-- With difference lists (show)

    -- shows L . (shows T . shows R)
    
    -- (shows LL . (shows LT . shows LR)) . (shows T . shows R)
    
    -- ((shows LLL . (shows LLT . shows LLR)) . (shows LT . shows LR)) . (shows T . shows R)

-- You still need to resolve three (.) until you get to the first character of the result string, but for the subsequent characters you do not need to resolve those dots. In the end, resolution of all (.) may need some time but then concatenation is performed entirely right-associative.

type ShowS = String -> String

hello = ("hello" ++)
world = ("world" ++)

-- We can "concatenate" ShowS values simply by composing them:
helloworld = hello . world

-- and turn them into Strings by passing them an empty list:
helloworld' = helloworld ""

-- showsPrec :: (Show a) => Int -> a -> ShowS