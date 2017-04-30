module CaseAlt where

import Data.List
import Data.Maybe

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

-- (?)
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

if_alt x = if' (x < 0) "233" $
           if' (x > 0) "2333" $
           "23333"

infixl 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'
trivium_alt :: (Eq a) => a -> String
trivium_alt x = x == x ? "Always True" $ "Always False"

{-# LANGUAGE MultiWayIf #-}
{-
multi_way x = if | x < 0 -> "233"
                 | x > 0 -> "2333"
-}

-- List comprehension
list_comprehension_alt x = head $
                            [ "233" | x < 0 ] ++
                            [ "2333" | x > 0] ++
                            [[]]

-- select
select :: a -> [(Bool, a)] -> a
select def = maybe def snd . Data.List.find fst
        -- = fromMaybe def . lookup True
        -- = maybe def id . lookup True
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a
select_example x = select "23333"
                    [(x < 0, "233"),
                     (x > 0, "2333")]