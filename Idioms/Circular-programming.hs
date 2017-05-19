-- Circular programming
-- ref: https://wiki.haskell.org/Circular_programming

--
--
--        THE LOOP OF THE ARROW --- BEING ITSELF ALSO AN ARROW
--                   
--          ,...............................................,
--          :                                               :
--          :                                               :
--          :                  +---------+                  :
--          :   INPUT          |:::::::::|         OUTPUT   :
--  ==========================>+:::::::::+========================>
--          :                  |::ARROW::|                  :
--          :            +====>+:::::::::+=====+            :
--          :            |     |:::::::::|     |            :
--          :            |     +---------+     |            :
--          :            |                     |            :
--          :            +==========<==========+            :
--          :                    FEEDBACK                   :
--          :                                               :
--          ;...............................................;
--

class Arrow arrow => ArrowLoop arrow where
        loop :: arrow (input, feedback) (output, feedback) -> arrow input output

tracea :: ((input, feedback) -> (output, feedback)) -> input -> output
tracea f input = let (output, feedback) = f (input, feedback)
                 in output

instance ArrowLoop (->) where
    loop = trace
{-
    input -- what we work with
    output -- what we get
    feedback -- a state-like thing, yes, a feedback, that can be thought of through both aspects
    feedback-as-produced: thinking of it as it leaves the box
    feedback-as-fed-back: thinking of it as it is fed back into the box again
-}
trace :: (input -> feedback -> (output, feedback)) -> input -> output
trace f input = let (output, feedback) = f input feedback
                in output

-- Let us see the tree minimum problem (described above), 
-- and let us modularize the magic circular step 
-- by this feedback view of trace:
repmin :: Ord a => Tree a -> Tree a
repmin = trace repIImin
 
repIImin :: Ord a => Tree a -> a -> (Tree a, a)
repIImin (Leaf minval) rep = (Leaf rep, minval)
repIImin (Branch left right) rep = let (left', min_left) = repIImin left rep
                                         (right', min_right) = repIImin right rep
                                   in (Branch left' right', min min_left min_right)

-- II means || parallelly             
{-
    input is a tree filled with numbers, a heterogenous tree
    output is the repmin'ed tree, a homogenized tree
    feedback (something state-like): here that strange numeric value plays the role of feedback! Its two aspects are
    feedback-as-produced -- in this aspect, it is a minimum value, a statistic of the tree
    feedback-as-fed-back -- in this aspect, it is a setter value, a rather seed-like thing: it is spred homogenously in the shape of a given tree
-}

--
--
--                How repmin is born: wrapping rep||min with trace
--
--                                    
--          ,.........................repmin............................,
--          :                                                           :
--          :                                                           :
--          :                     +------------+                        :
--          : Heterogenous tree   |::::::::::::|     Homogenized tree   :
--  =============================>+::::::::::::+================================>
--          :                     |::rep||min::|                        :
--          :               +====>+::::::::::::+=====+                  :
--          :               |     |::::::::::::|     |                  :
--          :    setter-val |     +------------+     |   min-val        :
--          :      a seed   |                        | a statistic      :
--          :               +===========<============+                  :
--          :                        FEEDBACK                           :
--          :                                                           :
--          ;...........................................................;
--

-- Normalizing vectors by trace
normalize :: Floating a => [a] -> [a]
normalize = trace divideIInorm
 
divideIInorm :: Floating a => [a] -> a -> ([a], a)
divideIInorm vector a = let (divided, norm2) = scaleIInorm2 vector (recip a)
                        in (divided, sqrt norm2)
 
scaleIInorm2 :: Floating a => [a] -> a -> ([a], a)
scaleIInorm2 [] _ = ([], 0)
scaleIInorm2 (x : xs) a = let (scaled, norm2) = scaleIInorm2 xs a
                          in (a * x : scaled, x * x + norm2)
{-
    trace requires a rather strict scheme: the function it takes as an argument must be exactly of the same scheme as seen in the above feedback picture
    the recursion by which we can solve the problem eventually (here: \mathrm{scale}\Vert\mathrm{norm}^2) usually provides another scheme. E.g. here we cannot compute norm directly by recursion.
-}

-- Decrease all elements by their average

diff :: Fractional a => [a] -> [a]
diff = trace decrementIIaverage
 
decrementIIaverage :: Fractional a => [a] -> a -> ([a], a)
decrementIIaverage list delta = let (decremented, sum', length') = decrementIIsumIIlength list delta
                                in (decremented, sum' `divideByInt` length')

decrementIIsumIIlength :: Fractional a => [a] -> a -> ([a], a, Integer)
decrementIIsumIIlength [] _ = ([], 0, 0)
decrementIIsumIIlength (a : as) delta = let (decremented, sum', length') = decrementIIsumIIlength as delta
                                        in (a - delta : decremented, a + sum', succ length')
 
divideByInt :: Fractional a => a -> Integer -> a
a `divideByInt` n = a / fromIntegral n

-- WhyAttributeGrammarMatter
-- ref: https://wiki.haskell.org/The_Monad.Reader/Issue4/Why_Attribute_Grammars_Matter