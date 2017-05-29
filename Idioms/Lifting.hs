-- Lifting
-- ref: https://wiki.haskell.org/Lifting
-- ref: https://wiki.haskell.org/Lifting_pattern

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
