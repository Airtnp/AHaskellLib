-- Wrapper types
-- ref: https://wiki.haskell.org/Wrapper_types

-- WrapperTypes are usually trivial wrappers (i.e. newtypes) that are designed to convey some information to the type system. Non-trivial type synonyms and Type class wrapper are both instances of this. This idiom is also in a synergistic relation to Phantom type, Traits type class, and Simulating dependent type.

{-

Usages and examples
One use of Wrapper types is to add Phantom type to a pre-existing (e.g. 3rd party) type.

Another example occurs in WxHaskell to handle interfacing to an OO library. The Sub-typing relationship is represented as nested wrapper types, so that type Labrador a = CAnimal (CDog (CLabrador a)) would represent a Labrador or a subclass of it, and bark :: CAnimal (CDog a) -> IO () would be a function that works for any CDog or subclass of it.
Traits type class and wrapper types often give two different approaches to solving the same problem. For example, if you want to compare two Strings for equality in different ways (mainly case-sensitive and case-insensitive) you can either use a wrapper to adapt String to the Eq class,

-}

newtype CIString = CIString String
 
instance Eq CIString where
    CIString a == CIString b = map toUpper a == map toUpper b

-- or you can make a TraitsTypeclass

class MyEq traits a where
    cmp :: traits -> a -> a -> Bool
 
data CaseSensitive
data CaseInsensitive
 
instance MyEq CaseSensitive String where cmp _ = (==)
instance MyEq CaseInsensitive String where cmp _ a b = map toUpper a == map toUpper b

{-

As the example illustrates, the two approaches have different trade-offs, but we can also get some of the benefits of both with the following synergy between Phantom type and Traits type class (and perhaps also wrapper types). What we do is store the traits type variable in a phantom type variable (added in this case via a wrapper type) which avoids the need for a Reified type parameter or the construction of a custom class (when the class already exists).

-}

newtype PString a = PString String
 
data CaseSensitive
data CaseInsensitive
 
instance Eq (PString CaseSensitive) where PString a = PString b = a == b
instance Eq (PString CaseInsensitive) where
    PString a == PString b = map toUpper a == map toUpper b

{-

This gives us the benefit of only having one type that we can parameterize to different implementations and the benefit of working with pre-existing type classes, it does however require us to provide a phantom type argument.

-}