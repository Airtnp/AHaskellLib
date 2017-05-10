{-# LANGUAGE FlexibleInstances #-}
module SimpleJSON (
        JValue(..),
        renderJValue,
        pretty,
        compact
    ) where

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Control.Arrow (second)

data JValue = 
    JString String
    | JNumber Double
    | JBool Bool
    | JNull
    | JObject (JObj JValue)
    | JArray (JAry JValue)
    deriving (Eq, Ord, Show)

data Doc = 
    Empty
    | Char Char
    | Text String
    | Line
    | Concat Doc Doc
    | Union Doc Doc
    deriving (Show)

type JError = String

-- avoid OverlappingInstances
newtype JAry a = JAry {
    fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
    fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "Not a JSON Boolean"

-- in Haskell 98, need LANGUAGE TypeSynonymInstances
-- instance cannot use [Char] instead of [] Char
instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _ = Left "Not a JSON String"

doubleToJValue :: (Double -> a) -> JValue -> Either JError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "Not a JSON Number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id

{- Overlappable
instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
-}

jAryFromJValue :: (JSON a) => JValue -> Either JError (JAry a)
jAryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jAryFromJValue _ = Left "Not a JSON Array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

jAryToJValue :: (JSON a) => JAry a -> JValue
jAryToJValue = JArray . JAry . map toJValue . fromJAry

instance (JSON a) => JSON (JAry a) where
    toJValue = jAryToJValue
    fromJValue = jAryFromJValue

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj
    
    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o) where 
        unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"    

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat xs = fold (<>) xs

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of 
    Just r -> text r
    Nothing | mustEscape c -> hexEscape c
            | otherwise -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'


simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/" where
    ch a b = (a, ['\\', b])

hexEscape :: Char -> Doc
hexEscape c 
    | d < 0x10000 = smallHex d
    | otherwise = astral (d - 0x10000)
    where d = ord c 


-- smallHex only support 0000-FFFF
smallHex :: Int -> Doc
smallHex x = text "\\u"
        <> text (replicate (4 - length h) '0')
        <> text h
    where h = showHex x "" 

-- fix 0000-10FFFF
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00) where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close f = enclose open close . fsep . punctuate (char ',') . map f

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x] where
    transform [] = ""
    transform (d:ds) = 
        case d of
            Empty -> transform ds
            Char c -> c : transform ds
            Text s -> s ++ transform ds
            Line -> '\n' : transform ds
            a `Concat` b -> transform (a:b:ds)
            _ `Union` b -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x] where
    best col (d:ds) = 
        case d of
            Empty -> best col ds
            Char c -> c : best (col+1) ds
            Text s -> s ++ best (col + length s) ds
            Line -> '\n' : best 0 ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b -> nicest col (best col (a:ds)) (best col (b:ds))
    best _ _ = ""

    nicest col a b | (width - least) `fits` a = a
                   | otherwise = b
                   where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w - 1) `fits` cs


getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull

renderJValue :: JValue -> Doc
renderJValue (JString s) = string s
renderJValue (JNumber n) = double n
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"

renderJValue (JArray a) = series '[' ']' renderJValue (fromJAry a)
renderJValue (JObject o) = series '{' '}' field (fromJObj o) where
    field (name, val) = string name <> text ": " <> renderJValue val


{-
renderJValue (JObject o) = "{" ++ pairs o ++ "}" where
        pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k, v) = show k ++ " " ++ renderJValue v
-}

{-
renderJValue (JArray a) = "[" ++ values a ++ "]" where
    values [] = ""
    values vs = intercalate ", " (map renderJValue vs)
-}


