{-# LANGUAGE Rank2Types #-}
--
module Main where

import Data.List
import Data.Function
-- import Control.Applicative -- Compiler slow

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
data PosTC = CartesianTC Double Double | PolarTC Double Double

-- deriving: auto-derive typeclass
data PosTCD = CartesianTCD Double Double | PolarTCD Double Double
    deriving Eq


{-
-- declare typeclass Eq (interface[java] / traits[rust] / require[c++] / CRTP[no need of /=])
class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    x == y = not (x /= y)

-- Interestingly, typeclass can auto-select interface...
class (Eq a) => Ord a where
    compare :: a -> a -> Ordering -- data Ordering = LT | EQ | GT
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
    compare x y | x == y = EQ
                | x <= y = LT
                | otherwise = GT
    max...
    min...
    <=...
    <...
    >=...
    >...
-}

-- declare PosTC as instance of Eq (realize (==))
instance Eq PosTC where
    CartesianTC x1 y1 == CartesianTC x2 y2 = (x1 == x2) && (y1 == y2)
    PolarTC x1 y1 == PolarTC x2 y2 = (x1 == x2) && (y1 == y2)
    CartesianTC x y == PolarTC a r = (x == r * cos a) && (y == r * sin a)
    PolarTC a r == CartesianTC x y = (x == r * cos a) && (y == r * sin a)
    
{-# LANGUAGE InstanceSigs #-}
{-
instance Eq A where
    (==) :: A -> A -> Bool
    x == y = ...
-}

-- can-derived typeclass
-- Eq/Ord/Enum/Bounded/Show/Read
-- GHC: Functor/Foldable/Traversable/Typeable/Generics

-- Show -> how to print
-- Read -> how to parse
read_alt :: (Read a) => String -> a
read_alt s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
                [x] -> x
                [] -> error "PreludeText.read: no parse"
                _ -> error "PreludeText.read: ambiguous parse"

-- read (show (x :: a)) :: a = x (May not True)

{- Ch8 Number Typeclass -}

-- Ord (default: lexicographical order)
-- compare/max/min/(<)/(<=)/(>=)/(>)

-- Never restrict data by typeclass

-- Enum
-- succ/pred/enumFrom/enumFromThen [x, y..]/enumFromTo [x..y]/enumFromThenTo [x, x'..y]
-- [m, n..l] is enum
data SPet = Dog | Cat | Bird | Turtle
            deriving (Enum, Show)
-- enumFromTo Dog Bird == [Dog, Cat, Bird]

-- Bounded
-- maxBound/minBound

-- Num
-- (+)/(-)/(*)/negate/abs/signum/fromInteger
-- Real/Fractional/RealFrac/Floating/Integral


{- Ch9 type/newtype/lazy-evaluation -}

-- type : aliasing
type IntList = [Int]
type IdT a = a -> a

id_alt :: a -> IdT a
id_alt a = id

-- newtype : like data
-- only-one constructor and zero abstraction overhead
-- no unboxing ctor 
-- must be lifted type
newtype Inch = Inch Double 
               deriving Eq
y = Inch 4

-- bottom(_|_) : cannot-be-calculated
undefined_alt = let x = x
                    in x

-- lifted type -> wrapper machine type (unlifted) so they can match _|_(⊥)
-- lazy thunk -> Tagless Graph

-- denotational semantics / referential transparency
--      every time eta-conversion a binding to expression -> value maintained
--      no side effect
--      compiler-friendly: inline/simplify/fusion easily

-- normal form/weak head normal form/thunk

-- eager evaluation -> weak head normal form
-- seq :: (a -> b -> b) seq = ... (Primitive)
-- ($!) :: (a -> b) -> a -> b

-- eager evaluation -> normal form
-- deepseq
-- force :: NFData a => a -> a

{- Ch10 Module -}
{-
module A.B.C (
    binding,
    module other,
    DataType(Ctor1, Ctor2...),  -- export one data DataTyoe
    AnotherData(..),
    ClassDef(class1, class2...) -- export one typeclass ClassDef
    ) where

import X.Y (Type, value...)
import qualified X.Y as Z  -- import as Z (or just use Y.xxx)
import qualified X.Y as Z hiding(Type, value...)

-}


{- Ch11 Functor -}

-- match maybe box
add_one_maybe :: Maybe Int -> Maybe Int
add_one_maybe (Just a) = Just (a + 1)
add_one_maybe Nothing = Nothing

-- To abstract box
-- class Functor f where
    -- fmap :: (a -> b) -> f a -> f b
    -- (<$) :: a -> f b -> f a
    -- (<$) :: fmap . const
-- instance Functor [] where
-- instance Functor Maybe where
-- instance Functor ((,), a) where   -- The box of (a, b) is (,) a
    -- fmap f (x, y) = (x, f y)
-- instance Functor ((->) a) where   -- The box of a -> b is (->) a
    -- fmap f fa = f . fa
    -- fmap = (.) [:t (b -> c) -> (a -> b) -> (a -> c)]

-- Everything is a box (container)
-- container is a functor

-- Some category theory
-- category C
    -- set class ob(C) : object
    -- set class hom(C) : projection
    -- binary operation o : combination of projection
        -- forall a b c : hom(b, c) x hom(a, b) -> hom(a, c)
        -- f : hom(a, b) o g : hom(b, c) = g o f
            -- associativity h o (g o f) = h o g
            -- identity forall x : object, exist idx : x -> x, forall f : a -> b, idb o f = f = f o ida

-- fmap: C -> D
    -- ob(C) -> ob(D)
    -- hom(C) - hom(D)
    -- fmap id (C) = id (D)
    -- fmap f o g = fmap(f) o fmap(g)  (isomorphic)
-- This operation is called lifted (a -> b) -> (f a -> f b)
-- or from one category -> another category

-- Identity : Id here
newtype Identity_alt a = Identity_alt { runIdentity_alt :: a }
instance Functor Identity_alt where
    fmap f idx = Identity_alt (f $ runIdentity_alt idx)
    -- or fmap f = Identity_alt . f . runIdentity_alt       -- point-free

-- Const : Nothing here
newtype Const_alt a b = Const_alt { getConst_alt :: a }  -- phatom type b
instance Functor (Const_alt a) where
    fmap f (Const_alt v) = Const_alt v      -- Error: Book P116

-- IO: a container(functor)


{- Ch12 lenses -}

-- without lenses
pr1 = MakePosR 1 2
pr2 = pr1 {getY = 3}
-- pr2 = MakePosR {
        -- getX = getX pr1
        -- getY = 3
--}
getX_PosR :: PosR -> Double
getX_PosR (MakePosR x _) = x

setX_PosR :: Double -> PosR -> PosR
setX_PosR x' p = p{ getX = x' }

-- with Lens (unwrapper your functor to apply object and wrapper into container)
-- LANGUAGE Rank2Types
type Lens_alt b a = forall f. Functor f => (a -> f a) -> b -> f b -- b.a exists
-- Then
xLens_PosR :: Functor f => (Double -> f Double) -> PosR -> f PosR
-- Or xLens_PosR :: Lens PosR Double
xLens_PosR f p = fmap (\x' -> setX_PosR x' p) $ f (getX_PosR p)
-- Or xLens_PosR = fmap (\x' -> p{ getX = x' }) & f (getX p)
-- getter: getX/setter: \x' -> p{ getX = x' }

-- Then
-- xLen_PosR (\x -> [x+1, x+2, x+3]) (MakePosR 3 4) -> [PosR{...}, PosR{...}, ...]

-- If we have
-- view_PosR :: Lens_alt PosR Double -> Double -> Double -- getter
-- set_PosR :: Lens_alt PosR Double -> Double -> PosR -> PosR -- setter
-- over_PosR :: Lens_alt PosR Double -> (Double -> Double) -> PosR -> PosR -- transformer

-- over
-- over :: Functor f => ((a -> f a) -> b -> f b) -> (a -> a) -> b -> b
-- over :: ((a -> Identity a) -> b -> Identity b) -> (a -> a) -> b -> b
over_alt :: Lens_alt b a -> (a -> a) -> b -> b
over_alt lens f x = runIdentity_alt $ lifted x  -- easy eta + beta => point-free
    where
        lifted = lens (Identity_alt . f)  -- b -> Indentity b

infixr 4 %~
(%~) :: Lens_alt b a -> (a -> a) -> b -> b
(%~) = over_alt


-- set
-- over lens (\_ -> a) p == set lens a p
-- set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set_alt :: Lens_alt b a -> a -> b -> b
set_alt lens a = over_alt lens (const a)

-- const :: a -> b -> a
-- const x _ = x

infixr 4 .~
(.~) :: Lens_alt b a -> a -> b -> b
(.~) = set_alt



-- view
-- view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view_alt :: Lens_alt b a -> b -> a
view_alt lens = getConst_alt . (lens Const_alt)


-- Then
-- yxLens = yLens . xLens 
-- view yxLens y -> y.x.T

infixl 8 %^
(%^) :: b -> Lens_alt b a -> a
x %^ lens = view_alt lens x
-- (^.) = flip view_alt

-- flip :: (a -> b -> c) -> b -> a -> c
-- flip x y = f y x

{- pipeline
infixl 1
(&) :: a -> (a -> b) -> b
x & f = f x
-}


{- Ch13 Applicative -}

-- concat :: [[a]] -> [a]
-- concat xss = foldl (++) [] xss

-- class Functor f => Applicative f where
    -- pure :: a -> f a
    -- (<*>) :: Functor f => f (a -> b) -> f a -> f b
    -- (<*)
    -- (*>)

    -- identity: pure id <*> v === v
    -- composition: pure (.) <*> u <*> v <*> w === u <*> (v <*> w)
    -- homomorphism: pure f <*> pure x === pure (f x)
    -- interchange: u <*> pure y === pure ($ y) <*> u
        -- ($ a) :: (a -> b) -> b

-- instance Applicative [] where  
    -- pure x = [x]
    -- fs <*> xs = [f x | f <- fs, x <- xs ]

-- instance Applicative ((->) a) where   -- (->): reader functor
    -- pure :: x -> (a -> x)
    -- pure x = \_ -> x -- just pure = const
    -- (<*>) :: (a -> (x -> y)) -> (a -> x) -> (a -> y)
    -- fxy <*> fx = \a -> fxy a $ fx a  -- fxy(a, fx(a))
    
-- pure (\x y z -> x + y + z) <*> (^2) <*> (^2) <*> (^3)
-- \x -> (x^2) + (x^3) + (x^4)
-- \x y z -> x + y + z :: (Num a) => a -> a -> a -> a
-- pure f :: Num a => const (a -> a -> a -> a) === (->) a (a -> a -> a)
-- g :: a -> a === (->) a (a)
-- f <*> g :: (->) a (a -> a)

-- natural lift
-- infixl 4 <$>
-- (<$>) :: (a -> b) -> f a -> f b
-- f <$> x = fmap f x
-- infixl -> function -> Functor
-- (+) <$> Just 1 <*> Just 2 == pure (+) <*> Just 1 <*> Just 2

-- helper

-- infixl 4 <$, $>
-- (<$) :: Functor f => a -> f b -> f a
-- (<$) = fmap . const
-- ($>) :: Functor f => f a -> b -> f b
-- ($>) = flip (<$)

-- infixl 4 <*, *>
-- (*>) :: Applicative f => f a -> f b -> f b
-- a1 *> a2 = (id <$ a1) <*> a2
-- (<*) :: Applicative f => f a -> f b -> f a  -- compare to const
-- (<*) = flip (*>)

-- Conclusion
-- <*> : horizonally concat function in category with its parameters in category
-- pure: 
    -- (global) lift function in any category defined by following parameters 
        -- pure :: Applicative f => a -> f a
    -- (instance): wrap function in one category
-- natural lift:
    -- f <$> x <*> y <*> ...
    -- fmap with many in-category parameters

{- Ch14 Monoid -}



main :: IO()
main = print(nub [1, 2, 3, 2, 3])  -- Prelude.base.print