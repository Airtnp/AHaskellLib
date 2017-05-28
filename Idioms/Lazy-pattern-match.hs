-- Lazy pattern match
-- ref: https://wiki.haskell.org/Lazy_pattern_match
-- Lazy functors
-- ref: https://wiki.haskell.org/Lazy_functors

-- lazy pattern match
-- These are all lazy pattern matches:

let (a,b) = p
f ~(a,b) = ...
case p of ~(a,b) -> ...
(\ ~(a,b) -> ... )

-- The let matches the top-most constructor lazily.

-- This seems to be quite arbitrary but this is how it is defined. That is, if you want to match constructors lazily in two levels then you have to write:

let (a, ~(b,c)) = p
f ~(a, ~(b,c)) = ...
case p of ~(a, ~(b,c)) -> ...
(\ ~(a, ~(b,c)) -> ... )

-- The lazy pattern match on a pair as in

f ~(a,b) = g a b
f p = g (fst p) (snd p)

-- Generally, a lazy pattern match is translated to calling corresponding record field accessors. The key difference between strict pattern match is that the strict pattern match requires to check for the pair constructor before g can be evaluated. In contrast to that, the lazy pattern match allows to defer the pair constructor match to the evaluation of g a b. If the function g can generate something without looking at its arguments then f can generate something as well before matching the pair constructor. This difference can be essential.

-- For example compare the implementation of splitAt with lazy and strict pattern match. Here is the (correct) lazy implementation:

import Prelude hiding (splitAt)
 
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs =
   if n<=0
     then ([], xs)
     else
        case xs of
           [] -> ([], [])
           y:ys ->
              case splitAt (n-1) ys of
                 ~(prefix, suffix) -> (y : prefix, suffix)

-- With lazy pattern match in the last line of the splitAt implementation you see an answer immediately whereas with a strict pattern match the Haskell interpreter requires some time and memory before showing something. The reason is that the strict pattern match forces the interpreter to perform all recursive calls to splitAt in order to check whether they actually generate a pair constructor. This might look silly since the pair type has only one constructor but mind you that a pair value can also be undefined. Furthermore this behavior is consistent with data types with more than one constructor. If you are uncertain how that splitAt magic works then it might help to translate the tilde into pair element accessors fst and snd.

-- The lazy pattern match has some consequences. First of all a lazy pattern matches immediately always. Remember,

f ~(x:xs) = x:xs
f ys = head ys : tail ys

-- ys always succeeds. 

-- fine but stupid, because the first match already requires the decision whether the list is empty or not. But the reversed order
f :: [a] -> [a]
f [] = []
f ~(x:xs) = x:xs

-- However

-- yields a compiler warning for an unnecessary match on [] because the first case already matches always. 
f :: [a] -> [a]
f ~(x:xs) = x:xs
f [] = []

-- You may have also noted the danger caused by a lazy pattern match. Since the lazy pattern match on a non-empty list is translated to head and tail it is as dangerous as head and tail, because these functions are not total. That is, generally it is good style to avoid lazy pattern matches on types with more than one constructor.

-- Lazy functor

data Pair a = Pair a a

-- whether lazy

instance Functor Pair where
   fmap f ~(Pair a b) = Pair (f a) (f b)
 
instance Applicative Pair where
   pure a = Pair a a
   ~(Pair fa fb) <*> ~(Pair a b) = Pair (fa a) (fb b)
 
instance Fold.Foldable Pair where
   foldMap = Trav.foldMapDefault
 
instance Trav.Traversable Pair where
   sequenceA ~(Pair a b) = liftA2 Pair a b

-- or strict

instance Functor Pair where
   fmap f (Pair a b) = Pair (f a) (f b)
 
instance Applicative Pair where
   pure a = Pair a a
   (Pair fa fb) <*> (Pair a b) = Pair (fa a) (fb b)
 
instance Fold.Foldable Pair where
   foldMap = Trav.foldMapDefault
 
instance Trav.Traversable Pair where
   sequenceA (Pair a b) = liftA2 Pair a b

-- Answer: deduce from applying to undefined values

{-

import Control.Monad.Identity (Identity(Identity))
 
fmap id x  ==  x
 
pure id <*> x  ==  x
f <*> pure x  ==  pure ($x) <*> f
 
sequenceA (fmap Identity x) = Identity x

-}

-- With the first definitions with lazy matching the laws are violated:

{-

    fmap id undefined  ==  Pair undefined undefined
    
    -- because of laziness in the second operand of <*> we get:
    pure id <*> undefined  ==  Pair undefined undefined
    
    -- if the second operand is matched strictly, and the first one lazily,
    -- then we get:
    undefined <*> pure undefined  ==  Pair undefined undefined
    pure ($ undefined) <*> undefined  ==  undefined
    
    -- given that fmap matches strict now, since lazy matching is incorrect
    sequenceA (fmap Identity undefined)  ==  Identity (Pair undefined undefined)

    -- In contrast to that the strict pattern matching is correct in this respect:

    fmap id undefined  ==  undefined
    pure id <*> undefined  ==  undefined
    undefined <*> pure undefined  ==  undefined
    pure ($ undefined) <*> undefined  ==  undefined
    sequenceA (fmap Identity undefined) = Identity undefined

-}