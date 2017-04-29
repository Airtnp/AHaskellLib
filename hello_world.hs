--
module Main where

import Data.List
import Data.Function

{-
basic comments
-}

{- Basics
    -- Type
    True
    sort [1, 2, 3, 4]
    case Foo of True  -> 1
                False -> 2

    -- let scope
    let x = 3 * a
        y = 4 * a
    in sqrt (x^2 + y^2)

    -- basic binding
    addOne :: Int -> Int
    addOne x = x + 1

    welcomeMsg :: String
    welcomeMsg = "Hello world"

    -- lexical scope
    let x = 1
        in let y = x * 2
            in x + y

    -- prefix <-> infix
    (+) 2 3 == 2 + 3
    l = [1] ++ [] ++ [2, 3, 4]
    elem 3 [1, 2, 3] == 3 `elem` [1, 2, 3]

    -- Precedence
    -- Simple Func > 0-9 infix
    -- infix/infixl/infixr (default `f` == infixl 9)
    -- 3 * -2 -- Error!  - : simple func neg | infix 6 sub
    -- :: the lowest precedence

-}

{- Ch2: Data and pattern-matching -}
-- data: declare T + declare T's constructor
data T = ConstructorT Int
    -- ConstructorT 1 :: T
ti :: T
ti = ConstructorT 1
-- or
data U = U Int
    -- U 1 :: U
u1 :: U
u1 = U 1
-- or
data V = Int :+ Int
    -- 1 :+ 2 :: V
    -- (:+) 1 2 :: V
v1 :: V
v1 = 1 :+ 2

v_norm2 :: V -> V -> Double
v_norm2 v1 v2 = case v1 of
                    x1 :+ y1 ->
                        case v2 of
                            x2 :+ y2 ->
                                sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))

v_norm2_alter :: V -> V -> Double
v_norm2_alter (x1 :+ y1) (x2 :+ y2) = sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))

v_norm2_alter2 :: V -> V -> Double
v_norm2_alter2 v1 v2 = let x1 :+ y1 = v1
                           x2 :+ y2 = v2
                       in sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))

v_norm2_pattern :: V -> V -> Double
v_norm2_pattern v1@(x1 :+ y1) v2@(x2 :+ y2) = sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))

-- multiple ctor
data Pos = Cartesian Double Double | Polar Double Double

-- no parameter ctor
data SelfBool = True | False
match_self_bool :: Bool -> Int
match_self_bool v = if v then 1
                         else 2

-- parametric polymorphism
data PosP a = PosP a a Double
posp1 :: PosP Int
posp1 = PosP 1 1 2.4
posp2 = PosP 2 2 3 :: PosP Double

-- maybe
int_div :: Int -> Int -> Double
int_div = (/) `on` fromIntegral

self_div :: Double -> Double -> Maybe Double
self_div a b = case b of
                 0 -> Nothing
                 _ -> Just (a / b)
                    
-- record syntax
data PosR = MakePosR { getX :: Double, getY :: Double }
-- data Vec = MakeVec { getX :: Int, getY :: Int } Multiple defintion
posr1 = MakePosR 3 4

{- Ch3: List/Recursion -}
-- data [a] = a : [a] | []
-- List a = Nil | a : List a
-- [1, 2, 3] -> 1 : 2 : 3 : []
sample_l1 = [10, 9..0] -- 10, 9 ... (If use [2, 9..0] -> empty list)
sample_p4 = sample_l1 !! 4 
-- !! partial function (!! [] n will cause error)
-- !!! total function
totaln :: [a] -> Int -> Maybe a
totaln [] _ = Nothing
totaln (x : xs) 0 = Just x
totaln (x : xs) n = totaln xs (n-1)
infixl 9 !!!
(!!!) :: [a] -> Int -> Maybe a
a !!! b = totaln a b


main :: IO()
main = print(nub [1, 2, 3, 2, 3])  -- Prelude.base.print