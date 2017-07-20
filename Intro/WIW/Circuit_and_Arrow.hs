-- ref: https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial
{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

-- Arrows which can save state
newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
      where
        (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
            let (cir1', b) = cir1 a
                (cir2', c) = cir2 b
            in  (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(b, d) ->
        let (cir', c) = cir b
        in  (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _   []     = []
runCircuit cir (x:xs) =
    let (cir',x') = unCircuit cir x
    in  x' : runCircuit cir' xs
-- or runCircuit cir = snd . mapAccumL unCircuit cir    

-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
    let (output, acc') = input `f` acc
    in  (accum acc' f, output)

-- | Accumulator that outputs the accumulator value.
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n

mean3 :: Fractional a => Circuit a a
mean3 = proc value -> do
    (t, n) <- (| (&&&) (total -< value) (total -< 1) |)
    returnA -< t / n

mean4 :: Fractional a => Circuit a a
mean4 = proc value -> do
    (t, n) <- (total -< value) &&& (total -< 1)
    returnA -< t / n

mean5 :: Fractional a => Circuit a a 
mean5 = proc value -> do
    rec
        (lastTot, lastN) <- delay (0,0) -< (tot, n)
        let (tot, n) = (lastTot + value, lastN + 1)
        let mean = tot / n
    returnA -< mean

-- proc
{-
proc is the keyword that introduces arrow notation, and it binds the arrow input to a pattern (value in this example). Arrow statements in a do block take one of these forms:
    variable binding pattern <- arrow -< pure expression giving arrow input
    arrow -< pure expression giving arrow input
-}

instance ArrowLoop Circuit where
    loop (Circuit cir) = Circuit $ \b ->
        let (cir', (c,d)) = cir (b,d)
        in  (loop cir', c)