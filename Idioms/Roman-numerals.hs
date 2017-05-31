-- Roman numerals
-- ref: https://wiki.haskell.org/Roman_numerals

-- The system of Roman numerals is a numeral system originating in ancient Rome, and was adapted from Etruscan numerals. The system used in classical antiquity was slightly modified in the Middle Ages to produce the system we use today. It is based on certain letters which are given values as numerals.

import Data.Maybe (fromJust)
 
romanToInt :: String -> Int
romanToInt = fst
                  . foldr (\p (t,s) -> if p >= s then (t+p,p) else (t-p,p)) (0,0)
                  . map (fromJust . flip lookup (zip "IVXLCDM" [1,5,10,50,100,500,1000]))

import System  (getArgs, getProgName)
import Numeric (readDec)
import Maybe   (fromJust, fromMaybe)
import Char    (toUpper)

{- Roman Numerals.  You can use toRoman and fromRoman directly
-- if you wish.  You can also compile this module as a program and
-- use it at the commandline under the names toRoman or fromRoman,
-- depending on the sense you want the conversion.  Note, there isn't
-- any error-checking on the input strings, and we use limited precision
-- Ints because surely no-one wants to play with roman numerals any
-- larger than that!

-- Author: Malcolm.Wallace@cs.york.ac.uk, 29 July 1999
-}

toRoman   :: Int -> String
fromRoman :: String -> Int


-- Each numeral has a decimal value.
numerals = [ ('I',   1), ('V',   5), ('X',  10), ('L',  50),
             ('C', 100), ('D', 500), ('M',1000) ]

-- For each numeral, there is a single permitted prefix digit for subtraction.
subnums  = [ ('V','I'),  ('X','I'),  ('L','X'),
             ('C','X'),  ('D','C'),  ('M','C') ]

-- Traverse the numeral list with an accumulator consisting of the
-- string built so far (in reverse order) and the remaining value to be
-- converted.
toRoman n  = (reverse . snd) (foldr toNumeral (n,"") numerals)

-- Each numeral could potentially appear many times (case 1), and we must
-- also handle (case 2) where a numeral *nearly* fits so we use a subtractive
-- prefix.
toNumeral st@(rdigit, base) (n,s)
  | n >= base    = toNumeral st (n-base, rdigit:s)
  | n+k >= base  = (n-base+k, rdigit:tdigit:s)
  | otherwise    = (n,s)
  where tdigit = fromMaybe '\0' (lookup rdigit subnums)
        k      = fromMaybe  0   (lookup tdigit numerals)



-- The inverse is pretty straightforward by comparison.  First, divide
-- up the string into chunks of identical letters, and add those together
-- (maxmunch).  Then accumulate these from the right - an intermediate
-- letter-sum which is less than the value already accumulated means it
-- must be a prefix subtraction (fromNumeral) rather than an addition.

fromRoman = foldr fromNumeral 0 . maxmunch . map toUpper
fromNumeral x y
  | x < y  = y-x
  | x > y  = y+x
maxmunch "" = []
maxmunch string@(x:_) =
  let (these,those) = span (x==) string
  in fromJust (lookup x numerals) * length these : maxmunch those



-- Now just some tidying up so we can call the program from the
-- commandline.

safeRead s =
  case readDec s of
    [] -> 0
    ((n,_):_) -> n

choose whoami =
  case normaliseProgName whoami of
    "toRoman"   -> toRoman . safeRead
    "fromRoman" -> show . fromRoman
    _           -> error "Usage: toRoman num ...\n       fromRoman LXIV ..."
  where
  normaliseProgName = reverse . takeWhile ('/'/=) . reverse
                      -- to strip any leading directory pathname.

main = do
    whoami <- getProgName
    args   <- getArgs
    (putStr . unlines . map (choose whoami)) args

--main	= getArgs >>= putStr . unlines . map (toRoman . safeRead)
--main	= getArgs >>= putStr . unlines . map (show . fromRoman)


{-# OPTIONS_GHC -fglasgow-exts #-}
module Romans where
 
class Roman t where
  roman :: t -> Int
 
data O   -- 0
data I a -- 1
data V a -- 5
data X a -- 10
data L a -- 50
data C a -- 100
data D a -- 500
data M a -- 1000
 
instance                Roman O         where roman _ = 0
instance                Roman (I O)     where roman _ = 1
instance                Roman (V O)     where roman _ = 5
instance                Roman (X O)     where roman _ = 10
 
instance Roman (I a) => Roman (I (I a)) where roman _ = roman (undefined :: (I a)) + 1
instance Roman a     => Roman (I (V a)) where roman _ = roman (undefined :: a)     + 4
instance Roman a     => Roman (I (X a)) where roman _ = roman (undefined :: a)     + 9
 
instance Roman (I a) => Roman (V (I a)) where roman _ = roman (undefined :: (I a)) + 5
instance Roman (V a) => Roman (V (V a)) where roman _ = roman (undefined :: (V a)) + 5
 
instance Roman (I a) => Roman (X (I a)) where roman _ = roman (undefined :: (I a)) + 10
instance Roman (V a) => Roman (X (V a)) where roman _ = roman (undefined :: (V a)) + 10
instance Roman (X a) => Roman (X (X a)) where roman _ = roman (undefined :: (X a)) + 10
instance Roman a     => Roman (X (L a)) where roman _ = roman (undefined :: a)     + 40
instance Roman a     => Roman (X (C a)) where roman _ = roman (undefined :: a)     + 90
instance Roman a     => Roman (X (D a)) where roman _ = roman (undefined :: a)     + 490
 
instance Roman a     => Roman (L a)     where roman _ = roman (undefined :: a)     + 50
 
instance Roman (I a) => Roman (C (I a)) where roman _ = roman (undefined :: (I a)) + 100
instance Roman (V a) => Roman (C (V a)) where roman _ = roman (undefined :: (V a)) + 100
instance Roman (X a) => Roman (C (X a)) where roman _ = roman (undefined :: (X a)) + 100
instance Roman (L a) => Roman (C (L a)) where roman _ = roman (undefined :: (L a)) + 100
instance Roman (C a) => Roman (C (C a)) where roman _ = roman (undefined :: (C a)) + 100
instance Roman a     => Roman (C (D a)) where roman _ = roman (undefined :: a)     + 400
instance Roman a     => Roman (C (M a)) where roman _ = roman (undefined :: a)     + 900
 
instance Roman a     => Roman (D a)     where roman _ = roman (undefined :: a)     + 500
 
instance Roman a     => Roman (M a)     where roman _ = roman (undefined :: a)     + 1000
 
-- Example type: XVI ~> X (V (I O)); MCMXCIX ~> M (C (M (X (C (I (X O))))))
 
powersoftwo = [roman (undefined :: (I (I O))),
               roman (undefined :: (I (V O))),
               roman (undefined :: (V (I (I (I O))))),
               roman (undefined :: (X (V (I O)))),
               roman (undefined :: (X (X (X (I (I O)))))),
               roman (undefined :: (L (X (I (V O))))),
               roman (undefined :: (C (X (X (V (I (I (I O)))))))),
               roman (undefined :: (C (C (L (V (I O)))))),
               roman (undefined :: (D (X (I (I O))))),
               roman (undefined :: (M (X (X (I (V O)))))),
               roman (undefined :: (M (M (X (L (V (I (I (I O)))))))))]

data RomanDigit a =
     O   -- 0
   | I a -- 1
   | V a -- 5
   | X a -- 10
   | L a -- 50
   | C a -- 100
   | D a -- 500
   | M a -- 1000