-- Integers too big for floats
-- ref: https://wiki.haskell.org/Integers_too_big_for_floats

-- factorial 777 is not representablt by Double, but is representable by Integer

-- However we need factorial 777 / factorial 778

{-

    Actually you can represent the fraction factorial 777 / factorial 778 as Rational
    and convert that to a floating point number:

    fromRational (factorial 777 % factorial 778)
    Fortunately fromRational is clever enough to handle big numerators and denominators.
    But there is an efficiency problem:

    Before fromRational can perform the imprecise division, the % operator will cancel the fraction precisely. You may use the Rational constructor :% instead.
    However that's a hack, since it is not sure that other operations work well on non-cancelled fractions.

    You had to import GHC.Real.
    But since we talk about efficiency let's go on to the next paragraph, where we talk about real performance.

-}

-- The problem is to compute the reciprocal of π using Chudnovsky's algorithm:

-- An exact division
-- Courtesy of Max Rabkin
(/.) :: (Real a, Fractional b) => a -> a -> b
x /. y = fromRational $ toRational x / toRational y
 
-- Compute n!
fac :: Integer -> Integer
fac n = product [1..n]
 
-- Compute n! / m! efficiently
facDiv :: Integer -> Integer -> Integer
facDiv n m 
    | n > m = product [n, n - 1 .. m + 1]
    | n == m = 1
    | otherwise = facDiv m n
 
 
-- Compute pi using the specified number of iterations
pi' :: Integer -> Double
pi' steps = 1.0 / (12.0 * s / f)
    where
      s = sum [chudnovsky n | n <- [0..steps]]
      f = fromIntegral c ** (3.0 / 2.0) -- Common factor in the sum
 
      -- k-th term of the Chudnovsky serie
      chudnovsky :: Integer -> Double
      chudnovsky k 
          | even k = num /. den
          | otherwise = -num /. den
          where
            num = (facDiv (6 * k) (3 * k)) * (a + b * k)
            den = (fac k) ^ 3 * (c ^ (3 * k))
 
      a = 13591409
      b = 545140134
      c = 640320
 
-- main = print $ pi' 1000

-- But assume these conversions are a problem. We will show a way to avoid them. The trick is to compute the terms incrementally. We do not need to compute the factorials from scratch for each term, instead we compute each term using the term before.

start :: Floating a => a
start =
   12 / sqrt 640320 ^ 3
 
arithmeticSeq :: Num a => [a]
arithmeticSeq =
   iterate (545140134+) 13591409
 
factors :: Floating a => [a]
factors =
   -- note canceling of product[(6*k+1)..6*(k+1)] / product[(3*k+1)..3*(k+1)]
   map (\k -> -(6*k+1)*(6*k+3)*(6*k+5)/(320160*(k+1))^3) $ iterate (1+) 0
 
summands :: Floating a => [a]
summands =
   zipWith (*) arithmeticSeq $ scanl (*) start factors
 
recipPi :: Floating a => a
recipPi =
   sum $ take 2 summands