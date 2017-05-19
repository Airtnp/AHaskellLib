-- Comparison chain
-- ref: https://wiki.haskell.org/Comparison_chain

-- no a <= x <= b
-- In languages like C the expression is parsed as (a <= x) <= b which is even worse. The first part is evaluated to a boolean value, which is then compared with b. (For C "boolean" and "integer" are the same type.)

isInRange :: Ord a => a -> a -> a -> Bool
isInRange lower upper x = lower <= x && x <= upper

(<?) :: Ord a => a -> (a,a) -> Bool
(<?) = flip (uncurry isInRange)

-- Then a <= x <= y === x <? (a, b)

-- More complex
{- * chains of relations (comparison, subsets, logical implications etc.) -}
 
infixr 4 &-, -&
 
type Rel   a = (a -> a -> Bool)
type Chain a = [(Rel a, a)]
 
endChain :: Chain a
endChain = []
 
-- separate comparison and operand
(&-) :: Rel a -> (a, Chain a) -> Chain a
rel &- (x,xs) = (rel,x):xs
 
-- separate operand and comparison
(-&) :: a -> Chain a -> (a, Chain a)
(-&) = (,)
 
-- check if all comparisons are true
check :: (a, Chain a) -> Bool
check (x,chain) =
   let (rels,xs) = unzip chain
   in  and (zipWith3 id rels (x:xs) xs)
 
 
 -- 1 < 5 < 10
example1 :: Bool
example1 =
   check (1 -& (<) &- 5 -& (==) &- 5 -& (<=) &- 10 -&
      (endChain :: Chain Integer))
 
 
{- * specialised infix operators for comparison -}
 
infixr 4 ==:, /=:, <:, >:, <=:, >=:
 
(==:), (/=:), (<:), (>:), (<=:), (>=:) :: Ord a =>
   a -> (a, Chain a) -> (a, Chain a)
(==:) = lift (==)
(/=:) = lift (/=)
(<:)  = lift (<)
(>:)  = lift (>)
(<=:) = lift (<=)
(>=:) = lift (>=)
 
lift :: Rel a -> a -> (a, Chain a) -> (a, Chain a)
lift f x (y,chain) = (x, (f,y):chain)

example2 :: Bool
example2 =
   check (1 <: 5 ==: 5 <=: 10
             -& (endChain :: Chain Integer))

-- You can represent a successful sequence of comparisons with Just a and a failed sequence with Nothing:
import Data.Maybe (isJust)
 
lift :: (a -> b -> Bool) -> Maybe a -> b -> Maybe b
lift f (Just x) y | f x y = Just y
lift _ _ _ = Nothing
 
(<:)  = lift (<)
(<=:) = lift (<=)
(*:)  = lift elem
 
example = isJust (Just 4 <: 5 <=: 6 *: [6,7])