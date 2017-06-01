-- Smart constructors
-- ref: https://wiki.haskell.org/Smart_constructors

-- This is an introduction to a programming idiom for placing extra constraints on the construction of values by using smart constructors.

-- Consider the following problem: we want to be able to specify a data type for electronic resistors. The resistors come in two forms, metal and ceramic. Resistors are labelled with a number of bands, from 4 to 8.

-- ensure only resistors with the right number of bands are constructed.

-- Dependent type

data Resistor = Metal   Bands
              | Ceramic Bands 
                deriving Show
 
type Bands = Int

metalResistor :: Bands -> Resistor
metalResistor n | n < 4 || n > 8 = error "Invalid number of resistor bands" 
                | otherwise      = Metal n
-- assertion

metalResistor :: Bands -> Resistor
metalResistor n = assert (n >= 4 && n <= 8) $ Metal n

-- Compile-time checking : the type system

-- Enforcing the constraint statically

-- There are other ways to obtain numerical checks like this. The most interesting are probably the static checks that can be done with Type arithmetic, that enforce the number of bands at compile time, rather than runtime, by lifting the band count into the type level.

data Z   = Z
data S a = S a

class Card c where
 
instance Card Z where
instance (Card c) => Card (S c) where

class Card size => InBounds size where
 
instance InBounds (S (S (S (S Z)))) where                 -- four
instance InBounds (S (S (S (S (S Z))))) where             -- five
instance InBounds (S (S (S (S (S (S Z)))))) where         -- six
instance InBounds (S (S (S (S (S (S (S Z))))))) where     -- seven
instance InBounds (S (S (S (S (S (S (S (S Z)))))))) where -- eight

data Resistor size = Resistor deriving Show

resistor :: InBounds size => size -> Resistor size
resistor _ = Resistor

-- By using a standard encoding of numeric values on the type level we are able to encode a bounds check in the type of a value, thus removing a runtime check, and removing the need to store the numeric value at runtime. The code is safer, as it is impossible to compile the program unless all resistors have the correct number of bands. An extension would be to use a decimal encoding for the integers (at the expense of longer code).

-- Further checks can be obtained by separating the metal and ceramic values on the type level, so no function that takes a metal resistor can be accidentally passed a ceramic one.


-- Runtime Optimisation : smart constructors

{-

    Another use for smart constructors is to perform basic optimisations, often to obtain a normal form for constructed data. For example, consider a data structure representing addition and multiplication of variables.

    data Expression = Variable String
                    | Add [Expression]
                    | Multiply [Expression]
    In this data structure, it is possible to represent a value such as Add [Variable "a", Add [Variable "b", Variable "c"]] more compactly as Add [Variable "a", Variable "b", Variable "c"].

    This can be done automatically with smart constructors such as:

    add :: [Expression] -> Expression
    add xs = Add (concatMap fromAdd xs)
    multiply :: [Expression] -> Expression
    multiply xs = Multiply (concatMap fromMultiply xs)
    
    fromAdd (Add xs) = xs
    fromAdd x = [x]
    fromMultiply (Multiply xs) = xs
    fromMultiply x = [x]

-}