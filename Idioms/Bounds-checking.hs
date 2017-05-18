-- Bounds checking
-- ref: https://wiki.haskell.org/Bounds_checking
-- paper: http://www.cs.cmu.edu/~fp/papers/pldi98dml.pdf
-- maillist: https://mail.haskell.org/pipermail/haskell/2004-August/014397.html

{-# OPTIONS -fglasgow-exts #-}
import Data.Array

bsearch cmp (key, arr) = brand arr (\arr' -> bsearch' cmp (key, arr'))
bsearch' cmp (key,arr) = look lo hi where
    (lo,hi) = bbounds arr
    look lo hi = let m = bmiddle lo hi
                    x = arr !. m
        in case cmp (key,x) of
            LT -> bpred lo m (look lo) Nothing
            EQ -> Just (unbi m, x)
            GT -> bsucc hi m (\m' -> look m' hi) Nothing

newtype BArray s i a = BArray (Array i a)
newtype BIndex s i   = BIndex i

unbi (BIndex i) = i

bbounds:: (Ix i) => BArray s i a -> (BIndex s i, BIndex s i)
bbounds (BArray a) = let (l,h) = bounds a in (BIndex l, BIndex h)

bmiddle:: (Integral i) => BIndex s i -> BIndex s i -> BIndex s i
bmiddle (BIndex i1) (BIndex i2) = BIndex ((i1 + i2) `div` 2)

bsucc:: (Ord i,Num i) 
	=> BIndex s i -> BIndex s i -> (BIndex s i -> r) -> r -> r
bsucc (BIndex upb) (BIndex i) on_within on_out
	= let i'    = i + 1
	  in if i' <= upb then (on_within (BIndex i')) else on_out

bpred:: (Ord i,Num i)
	=> BIndex s i -> BIndex s i -> (BIndex s i -> r) -> r -> r
bpred (BIndex lwb) (BIndex i) on_within on_out
	= let i'    = i - 1
	  in if i' >= lwb then (on_within (BIndex i')) else on_out

infixl 5 !.
(!.):: (Ix i) => BArray s i e -> (BIndex s i) -> e
(BArray a) !. (BIndex i) = a ! i

brand:: (Ix i) => Array i e -> (forall s. BArray s i e -> w) -> w
brand (a::Array i e) k = k ((BArray a)::BArray () i e)