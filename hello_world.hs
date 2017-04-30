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
data SelfBool = STrue | SFalse
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
-- (:) :: a -> [a] -> [a]

-- !! partial function (!! [] n will cause error)
-- !!! total function
totaln :: [a] -> Int -> Maybe a
totaln [] _ = Nothing
totaln (x : xs) 0 = Just x
totaln (x : xs) n = totaln xs (n-1)
infixl 9 !!!
(!!!) :: [a] -> Int -> Maybe a -- section infix -> simple
a !!! b = totaln a b

{- Ch4, Ch5: Tuple/Type Inference/High-order Function -}
-- data (,) a b = (,) a b  Tuple, max 62
tp1 = (1, 2, 3) :: (Int, Float, Double)
tp2 = (,,) 1 2 3 :: (Int, Float, Double)
unit_t = () :: () -- von Neumann Peano

-- high order
-- (->) :: * -> * -> *  (infixr 0) Parameters: Types, Return: Types -> Type Function
subscbfiveimpl :: [(Int, a)] -> [a]
subscbfiveimpl [] = []
subscbfiveimpl ((i, x) : xs) = case i `rem` 5 of
                                    0 -> x : subscbfiveimpl xs
                                    _ -> subscbfiveimpl xs

-- zip
zip_alt :: [a] -> [b] -> [(a, b)]
zip_alt [] _ = []
zip_alt _ [] = []
zip_alt (x:xs) (y:ys) = (x, y) : zip_alt xs ys

subscbfive :: [a] -> [a]
subscbfive [] = []
subscbfive xs = subscbfiveimpl (zip_alt [1..(length xs)] xs)

zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
zip_with _ _ [] = []
zip_with _ [] _ = []
zip_with f (x : xs) (y : ys) = f x y : zip_with f xs ys

-- curry
curry_alt :: ((a, b) -> c) -> a -> b -> c
curry_alt f x y = f (x, y)

uncurry_alt :: (a -> b -> c) -> (a, b) -> c
uncurry_alt f (x, y) = f x y

-- $ &
-- ($) :: (a -> b) -> a -> b (infixr 0)
-- f $ x = f x
-- replace f (x y z) -> f $ x $ y z
-- just pipeline
--      |> = &
--      <| = $

-- lambda function
sample_lambda1v = 3 & (\x -> x + 1) & (\x -> x + 2) -- 6

-- .
-- (.) :: (b -> c) -> (a -> b) -> a -> c  -- Or BCKW B
-- f . g = \x -> f(g x) (infixr 9)
from_a_to_b = toEnum . (+1) . fromEnum $ 'a' :: Char -- 'b'

-- where
where_func :: [a] -> [a]
where_func xs = a_func $ zip [0..] xs
    where
        a_func [] = []
        a_func ((i, x):xs) = if i `rem` 5 == 0 then x : a_func xs
                                               else a_func xs
-- everywhere binding happen can use where
-- eg: case ... of ... where ... | let ... where ... in ...

-- Alt case conditition : ref: https://wiki.haskell.org/Case#Guards
-- guard
guard_func :: (Num a, Ord a, Eq a) => a -> String
guard_func x
    | x' < 0 = "negative"
    | x' == 0 = "zero"
    | x' > 0 = "positive"
    | otherwise = "hodouni"
    where x' = x

multi_way_alt :: (Eq a) => a -> String
multi_way_alt x = case () of _
                               | x == x -> "Always True"
                               | otherwise -> "Always False"

-- point-free (eta-conversion in λ)
-- α-conversion : \x f x -> \y f y
-- β-reduction : \x f x z -> f z
-- η-conversion : \x f x == \x g x -> f == g

nextChar_less :: Char -> Char
nextChar_less = toEnum . (+1) . fromEnum
nextChar_full :: Char -> Char
nextChar_full = toEnum . (+1) . fromEnum

-- (.) . (.) :: (b -> c) -> (a1 -> a -> c) -> a1 -> a -> c
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- (*) `on` f = \x y -> f x * f y

{- Ch6 List Operation -}

-- map
-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x : xs) = f x : map f xs 

-- filter
-- filter f (x : xs) = | f x = x : filter f xs
                    -- |     = filter f xs

-- foldl (not work on infinite lists)
-- foldr (work on infinite lists)
foldr_alt :: (b -> a -> a) -> a -> [b] -> a
foldr_alt _ a [] = a
foldr_alt f a (x:xs) = f x $ foldr_alt f a xs

foldl_alt :: (a -> b -> a) -> a -> [b] -> a
foldl_alt _ a [] = a
foldl_alt f a (x:xs) = foldl_alt f (f a x) xs

foldl_reverse :: (a -> b -> a) -> a -> [b] -> a
foldl_reverse f a bs = foldr_alt (\b' a' -> f a' b') a (reverse bs)
-- save it on the closure, thus gf must match (b -> x -> x) -> x -> [b] -> x
-- x :: a -> a
-- gf :: b -> (a -> a) -> (a -> a)
foldl_foldr :: (a -> b -> a) -> a -> [b] -> a
foldl_foldr f a bs = foldr_alt (\b g x -> g $ f x b) id bs a
-- foldl f a bs = f (f z b0) b1 
--              -> foldr gf id bs a
--              -> gf b0 (gf b1 id) a
--              -> gf b1 id (f a b0)
--              -> id $ f (f a b0) b1

-- for finite lists
foldr_foldl :: (b -> a -> a) -> a -> [b] -> a  
foldr_foldl f a bs = foldl_alt (\g b x -> g $ f b x) id bs a

-- use foldl' instead, since foldl is lazy (evaluated at final saturation)

-- scanl
scanl_alt :: (b -> a -> b) -> b -> [a] -> [b]
scanl_alt f b ls = b : (
    case ls of
        [] -> []
        x:xs -> scanl f (f b x) xs
    )

scanr_alt :: (a -> b -> b) -> b -> [a] -> [b]
scanr_alt f b (x:xs) = 
        f x (head ys) : ys
    where
        ys = scanr_alt f b xs

{- Ch7 Typeclass -}



main :: IO()
main = print(nub [1, 2, 3, 2, 3])  -- Prelude.base.print