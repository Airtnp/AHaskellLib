-- Lifting
-- ref: https://wiki.haskell.org/Lifting
-- ref: https://wiki.haskell.org/Lifting_pattern
-- ref: https://wiki.haskell.org/Worker_wrapper


-- transform a function into a corresponding function within another (usually more general) setting (category).
-- hom in difference category

data Pair a = Pair a a
    deriving (Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

lift2 :: (a -> b -> r) -> (Pair a -> Pair b -> Pair r)
lift2 f (Pair x1 x2) (Pair y1 y2) = Pair (f x1 y1) (f x2 y2)

class Functor f => Liftable f where
    zipL :: f a -> f b -> f (a, b)
    zeroL :: f ()
 
liftL :: Liftable f => (a -> b) -> (f a -> f b)
liftL = fmap
 
liftL2 :: Liftable f => (a -> b -> c) -> (f a -> f b -> f c)
liftL2 f x y = fmap (uncurry f) $ zipL x y
 
liftL3 :: Liftable f => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
liftL3 f x y z = fmap (uncurry . uncurry $ f) $ zipL (zipL x y) z
 
liftL0 :: Liftable f => a -> f a
liftL0 x = fmap (const x) zeroL 
 
appL :: Liftable f => f (a -> b) -> f a -> f b
appL = liftL2 ($)

assoc :: ((a, b), c) -> (a, (b, c))
assoc ~(~(x, y), z) = (x, (y, z))

-- Liftable = Applicative
-- zeroL = pure ()
-- zipL = liftA2 (.)

-- In principle, Applicative should be a superclass of Monad, but chronologically Functor and Monad were before Applicative. Unfortunately, inserting Applicative between Functor and Monad in the subclass hierarchy would break a lot of existing code and thus has not been done as of today (2011). This is still true as of Jan 2013. GHC 7+ fix it

return  :: (Monad m) => a -> m a
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

plus :: [Int] -> [Int] -> [Int]
plus = liftM2 (+)
-- plus [1,2,3] [3,6,9] ---> [4,7,10, 5,8,11, 6,9,12]
-- plus [1..] []        ---> _|_ (i.e., keeps on calculating forever)
-- plus [] [1..]        ---> []

{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE AllowUndecidableInstances #-}
import Control.Monad
 
instance (Functor m, Monad m) => Liftable m where 
    zipL  = liftM2 (\x y -> (x,y))
    zeroL = return ()

class MonadTrans t where
    lift :: Monad m => m a -> t m a -- lifts a value from the inner monad m to the transformed monad t m
                                    -- could be called lift0

-- Until now, we have only considered lifting from functions to other functions. John Hughes' arrows (see Understanding arrows) are a generalization of computation that aren't functions anymore. An arrow a b c stands for a computation which transforms values of type b to values of type c. The basic primitive arr, aka 
-- pure
arr :: (Arrow a) => (b -> c) -> a b c

-- is also a lifting operation.

-- Lambda Lifting

-- The lifting pattern is to float bindings to a higher level in the program. You can always do this if the function is a Combinator. You can convert non-combinators into combinators using Lambda lifting.

-- This is also known as Let floating.

reverse xs = rev' xs []
 where
  rev' [] ys = ys
  rev' (x:xs) ys = rev' xs (x:ys)

-- becomes:

reverse xs = rev' xs []
 
rev' [] ys = ys
rev' (x:xs) ys = rev' xs (x:ys)

-- A major reason is that sometimes bindings are sufficiently generic that they are useful in other contexts. You can avoid rewriting common things by lifting them. The prelude contains many things that could have started out as helper functions at one point but which have been lifted to the program and finally to the language level due to the fact that they are generically useful. Especially the many simple but useful list functions. Indeed, if one is not aware of what is in the prelude, one will probably end up writing at least a few of those on one's own. 

-- Another reason is testability. In the first version, you can't directly test the unlifted function (say, using one of the Unit testing frameworks). By lifting the definition to the outer level, you can directly test it.

-- Worker wrapper

-- It is sometimes easier or more efficient to write functions which have particular "start arguments" or that pass state. When this is the case write wrappers rather than trying to code within the original signature.

{-

Accumulator examples
e.g. the function reverse

reverse :: [a] -> [a]
could be written

reverse [] = []
reverse (x:xs) =  reverse xs ++ [x]
however this will be more efficient if it were written

reverse xs = revWorker [] xs
 
revWorker s [] = s
revWorker s (x:xs) = revWorker (x:s) xs
note often a worker will also return some state along with a result which can be stripped away.

revWorker could be considered a generally useful function. revWorker x y is equivalent to, but probably faster than, reverse y ++ x.

Other examples
Wrappers are often used to play the role of loop initialisation in imperative languages. For example:

fib n = fibWorker n 0 1
 
fibWorker n f1 f2
 | n == 0    = f1
 | otherwise = fibWorker (n-1) f2 (f1+f2)

Hiding the worker
Also, often one hides the worker(s) with a where (or let). E.g. :

fib = fibWorker 0 1
  where
  fibWorker f0 f1 n
    | n == 0  = f0
    | True    = fibWorker f1 (f0 + f1) (n-1)
Of course, one then has to de-hide the worker if one want to test it with different initial arguments.

In some cases, though, the worker can be a generally useful function on its own merits, in that case one obviously shouldn't hide it.


One common case is where you want to subject the worker function to Unit testing. In such a situation, the test suite has to be able to get to the worker function. Another is where the worker might be a candidate for further abstraction. (See Higher order function for examples.)

Moving a worker to a higher level (which may require some Lambda lifting) is known as Let floating.

-}