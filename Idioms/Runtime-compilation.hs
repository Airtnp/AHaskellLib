-- Runtime compilation
-- ref: https://wiki.haskell.org/Runtime_compilation

-- Many algorithms require a pre-processing step which builds some data structure for later use in the algorithm proper. Consider making this pre-processing step build a Haskell function instead. In other words, use functions not data structures.

-- Consider, for example, Knuth-Morris-Pratt substring searching. In a conventional language, the approach would be to compile the string to be searched for into an array of overlaps (this is the pre-processing step) which the search algorithm then uses to actually perform the match. One benefit is that if you need to search for the same substring multiple times, you can share the pre-processing step.

-- However, consider how you'd implement substring searching if speed were crucial and the string were fixed at compile time. Suppose, for example, you wanted to search for "aab". You might write something like this (note that this uses the Not just Maybe idiom):

search :: (Monad m) => String -> m (String, String)
search cs
    = search_aab [] cs
    where
        search_fail = fail "can't find aab"
 
        search_aab prev [] = search_fail
        search_aab prev (c@'a':cs)
            = search_ab (c:prev) cs
        search_aab prev (c:cs)
            = search_aab (c:prev) cs
 
        search_ab prev [] = search_fail
        search_ab prev (c@'a':cs)
            = search_b (c:prev) cs
        search_ab prev (c:cs)
            = search_aab prev (c:cs)
 
        search_b prev [] = search_fail
        search_b prev (c@'b':cs)
            = return (reverse (c:prev), cs)
        search_b prev (c:cs)
            = search_ab prev (c:cs)	-- Note special case here

-- This Haskell function builds the KMP overlap table:

overlap :: (Eq a) => [a] -> [Int]
overlap str
    = overlap' [0] str
    where
        overlap' prev []
            = reverse prev
        overlap' prev (x:xs)
            = let get_o o
                | o <= 1 || str !! (o-2) == x = o
                | otherwise = get_o (1 + prev !! (length prev - o + 1))
              in overlap' (get_o (head prev + 1):prev) xs

-- Then using this, we build up a Haskell function using continuation passing style to handle the failure states:

matchKMP :: (Monad m, Eq a) => [a] -> ([a] -> m ([a],[a]))
matchKMP []
    = error "Can't match empty list"
matchKMP xs
    = matchfunc []
    where
        matchfunc = makeMatchFunc [dofail] (zip xs (overlap xs))
        dofail = \ps xs -> case xs of
                    [] -> fail "can't match"
                    (y:ys) -> matchfunc (y:ps) ys
 
type PartialMatchFunc m a = [a] -> [a] -> m ([a], [a])
 
makeMatchFunc :: (Monad m, Eq a) => [PartialMatchFunc m a] -> [(a, Int)]
                    -> PartialMatchFunc m a
makeMatchFunc prev []
    = \ps xs -> return (reverse ps, xs)
makeMatchFunc prev ((x,failstate):ms)
    = thisf
    where
        mf = makeMatchFunc (thisf:prev) ms
        failcont = prev !! (length prev - failstate - 1)
        thisf = \ps xs -> case xs of
                        [] -> fail "can't match"
                        (y:ys) -> if (x == y) then mf (y:ps) ys
                                else failcont ps xs

{-

    We can now either use it as a standard substring match function:

    matchKMP "aab" "babaaba"
    or we can use currying to share the compiled function:

    match_aab :: (Monad m) => String -> m (String, String)
    match_aab = matchKMP "aab"
    This latter example only compiles the KMP machine once and, thanks to lazy evaluation, it's only compiled if it's ever used.

    Getting recursion into your generated code can be hard. The above example was relatively simple, but in the general case you may need something more sophisticated.

    See tying the knot for some ideas.

    Caveat: This technique is considered by many to be a hack at best, and a kludge at worst. Things can get very fiddly and extremely hard to debug. Runtime compilation can sometimes win you significant efficiency gains, but can often win you almost nothing at the cost of the your increased stress and reduced productivity.

    It's almost always worth implementing your algorithm the naive way first and only then if it's found to be the bottleneck, hand-compiling a few examples first to see if that speeds things up sufficiently.

    You have been warned.

-}