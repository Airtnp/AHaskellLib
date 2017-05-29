-- Prelude extensions
-- ref: https://wiki.haskell.org/Prelude_extensions

1 Tuples
-- It is often necessary to apply functions to either the first or the second part of a pair. This is often considered a form of mapping (like map from Data.List).

-- | Apply a function to the first element of a pair
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

-- | Apply a function to the second element of a pair
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- | Apply a function to both elements of a pair
mapPair :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (f, g) (a, b) = (f a, g b)

-- Data.Graph.Inductive.Query.Monad module (section Additional Graph Utilities) contains mapFst, mapSnd, and also a function >< corresponding to mapPair. Another implementation of these functions in the standard libraries: using first, second, *** arrow operations overloaded for functions (as special arrows), see Control.Arrow module, or Arrow HaskellWiki page.

-- 1.1 Treating pairs and lists in the same way
-- We can define a Pair class which allows us to process both pairs and non-empty lists using the same operator:

import Control.Arrow ((***))
 
infixl 4 <**>
 
class Pair p x y | p -> x, p -> y where
    toPair :: p -> (x, y)
    (<**>) :: (x -> a -> b) -> (y -> a) -> p -> b
    (<**>) f g = uncurry id . (f *** g) . toPair
 
instance Pair (a, b) a b where
    toPair = id
 
instance Pair [a] a [a] where
    toPair l = (head l, tail l)

-- 2 Matrices
--A simple representation of matrices is as lists of lists of numbers:

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)

-- These matrices may be made an instance of Num (though the definitions of abs and signum are just fillers):

instance Num a => Num (Matrix a) where
    Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
    Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
    Matrix as * Matrix bs =
        Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
    negate (Matrix as) = Matrix (map (map negate) as)
    fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
    abs m = m
    signum _ = 1

-- The fromInteger method builds an infinite matrix, but addition and subtraction work even with infinite matrices, and multiplication works as long as either the first matrix is of finite width or the second is of finite height.

-- Applying the linear transformation defined by a matrix to a vector is

apply :: Num a => Matrix a -> [a] -> [a]
apply (Matrix as) b = [sum (zipWith (*) a b) | a <- as]


-- 3 Data.Either extensions
import Data.Either
 
either', trigger, trigger_, switch ::  (a -> b) -> (a -> b) -> Either a a -> Either b b
 
either' f g (Left x) = Left (f x)
either' f g (Right x) = Right (g x)
 
trigger f g (Left x) = Left (f x)
trigger f g (Right x) = Left (g x)
 
trigger_ f g (Left x) = Right (f x)
trigger_ f g (Right x) = Right (g x)
 
switch f g (Left x) = Right (f x)
switch f g (Right x) = Left (g x)
 
sure :: (a->b) -> Either a a -> b
sure f = either f f
 
sure' :: (a->b) -> Either a a -> Either b b
sure' f = either' f f