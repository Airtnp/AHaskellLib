-- Heterogenous collections
-- ref: https://wiki.haskell.org/Heterogenous_collections
-- Oleg(HList): http://okmij.org/ftp/Haskell/HList-ext.pdf

{-

    Tuples	                        Lists
    Heterogeneous	                Homogeneous
    Fixed length (per tuple type)	Variable length
    Always finite	                May be infinite

-}

-- However, the need is for heterogeneous and non-fixed length. When one is used to Java, with its loose typing of collections,not having this immediately and easily available seems strange. As an example, the need is for something like LinkedList<Object> from Java. (vector<any> in Cpp)

-- ADT
data T
   = ConsInt    Int
   | ConsString String
   | ConsChar   Char

data Object = IntObject Int | StringObject String
 
objectString :: Object -> String
objectString (IntObject v) = show v
objectString (StringObject v) = v
 
-- main = mapM_ (putStrLn . objectString) [(IntObject 7), (StringObject "eight")]

-- A Universal type (Dynamic, Type-erase)

import Data.Dynamic
import Data.Maybe
 
-- 
-- A list of dynamic 
--
hlist :: [Dynamic]
hlist = [ toDyn "string"
        , toDyn (7 :: Int)
        , toDyn (pi :: Double)
        , toDyn 'x'
        , toDyn ((), Just "foo")
        ]
 
dyn :: Dynamic 
dyn = hlist !! 1

--
-- unwrap the dynamic value, checking the type at runtime
--
v :: Int
v = case fromDynamic dyn of
        Nothing -> error "Type mismatch"
        Just x  -> x

-- Existential types

-- The most convenient way to pack a value with its methods is to use a typeclass dictionary. The typeclass declaration defines the api to be wrapped with each value. You can also pack up your own interface as an explicit field in the data type, if you want to avoid type classes.
    
{-# LANGUAGE ExistentialQuantification #-}
--
-- An existential type encapsulating types that can be Shown
-- The interface to the type is held in the show method dictionary
--
-- Create your own typeclass for packing up other interfaces
--
data Showable = forall a . Show a => MkShowable a
 
--
-- And a nice existential builder
--
pack :: Show a => a -> Showable
pack = MkShowable
 
--
-- A heteoregenous list of Showable values
--
hlist :: [Showable]
hlist = [ pack 3
        , pack 'x'
        , pack pi
        , pack "string"
        , pack (Just ()) ]
 
--
-- The only thing we can do to Showable values is show them
--
-- main :: IO ()
-- main = print $ map f hlist
    where
        f (MkShowable a) = show a
 
{-
 
*Main> main
["3","'x'","3.141592653589793","\"string\"","Just ()"]
 
-}

-- One can of course make the type Showable an instance of the type class Show itself
--
-- Make Showable itself an instance of Show
--
instance Show Showable
  where
  showsPrec p (MkShowable a) = showsPrec p a
 
--
-- The only thing we can do to Showable values is show them
--
-- main :: IO ()
-- main = print hlist
 
{-
*Main> main
[3,'x',3.14159265358979,"string",Just ()]
-}

-- HLists, OOHaskell, type-level programming

    -- First Class Labels
    -- proposal: https://prime.haskell.org/wiki/FirstClassLabels

    -- Extensible record
    -- proposal: https://wiki.haskell.org/Extensible_record

