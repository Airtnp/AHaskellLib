-- Converting numbers
-- ref: https://wiki.haskell.org/Converting_numbers

-- 1. Converting from and between integral types (integer-like types)

-- Integer, which are arbitrary-precision integers, often called "bignum" or "big-integers" in other languages, and
-- Int, which fixed-width machine-specific integers with a minimum guaranteed range of −229 to 229 − 1. In practice, its range can be much larger: on the x86-64 version of Glasgow Haskell Compiler, it can store any signed 64-bit integer.

-- fromIntegral :: (Num b, Integral a) => a -> b
-- fromInteger :: Num a => Integer -> a
-- toInteger:: Integral a => a -> Integer

-- 2. Converting from real and between real-fractional types (rational-like types)

-- Rational, which are arbitrary-precision fractions, and
-- Double, which are double-precision floating-point numbers.

-- realToFrac:: (Real a, Fractional b) => a -> b
-- fromRational :: Fractional a => Rational -> a
-- toRational :: Real a => a -> Rational


-- 3. Converting from real-fractional numbers to integral numbers

-- ceiling  :: (RealFrac a, Integral b) => a -> b
-- floor    :: (RealFrac a, Integral b) => a -> b
-- truncate :: (RealFrac a, Integral b) => a -> b
-- round    :: (RealFrac a, Integral b) => a -> b

-- 4. Converting between different floating-point precisions

-- Avoid using realToFrac to convert between floating-point types as the intermediate type Rational is unable to represent exceptional values like infinity or NaN


-- float2Double :: Float -> Double
-- double2Float :: Double -> Float

-- 5. Automatic conversion
-- Generic number type

data GenericNumber =
    Integer Integer
  | Rational Rational
  | Double Double

-- no precision - floating point
-- no type-safety
-- no type-class

average :: Fractional a => [a] -> a
average xs = sum xs / genericLength xs

isSquare :: GenericNumber -> Bool
isSquare n = (round . sqrt $ n) ^ 2 == n

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters