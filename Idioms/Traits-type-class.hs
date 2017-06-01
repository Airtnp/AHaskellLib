-- Traits type class
-- ref: https://wiki.haskell.org/Traits_type_class
-- blog: http://augustss.blogspot.nl/2007/04/overloading-haskell-numbers-part-3.html

-- Occasionally you want to associate information with a type, not just a value. An example is the standard Bounded class:

class  Bounded a  where
    minBound :: a
    maxBound :: a

-- However, this technique does not work if the information which you want to extract doesn't have the type in its signature. One example is floating point format information, such as:

class FloatTraits a where
    -- This one is fine
    epsilon :: a
    -- This one is not
    mantissaDigits :: Int
    -- etc

-- The problem is that there is simply no way to tell Haskell which version of mantissaDigits you want, because the Type parameter a does not appear in its type signature anywhere.

-- The solution is to pass a Reified type as a phantom argument:

class FloatTraits a where
    mantissaDigits :: a -> Int
 
instance FloatTraits Float where
    mantissaDigits _ = 24
 
instance FloatTraits Double where
    mantissaDigits _ = 53

-- This technique works well in conjunction with Functional dependencies. For example, there may be some float types for which an Int may not be sufficient to express the number of digits in the mantissa:

class (Integral i) => FloatTraits a i | a -> i where
    mantissaDigits :: a -> i
 
instance FloatTraits Float Int where
    mantissaDigits _ = 24
 
instance FloatTraits ArbitraryPrecisionFloat Integer where
    mantissaDigits x = {- detail omitted -}

-- You can also use this technique as an alternative to Higher order functions in cases where you need several functions which work together.

-- Consider, for example, converting strings from upper case to lower case and back again. This in general depends on the language that you are operating in. The lower case version of 'A', for example, is different in English than in Greek. You can wrap this up in a typeclass parameterised on language:

class CaseConvert language where
    toUpperCase :: language -> String -> String
    toLowerCase :: language -> String -> String
 
data EnglishLanguage = EnglishLanguage
 
instance CaseConvert EnglishLanguage where
    toUpperCase _ s = {- etc -}
    toLowerCase _ s = {- etc -}
 
data GreekLanguage = GreekLanguage
 
instance CaseConvert GreekLanguage where
    toUpperCase _ s = {- etc -}
    toLowerCase _ s = {- etc -}