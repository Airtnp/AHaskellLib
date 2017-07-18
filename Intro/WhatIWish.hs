-- What I Wish I Knew When Learning Haskell

-- non-exhaustive
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Debug.Trace
-- trace/traceShow/tracePrintf
-- trace :: String -> a -> a

-- Type holes

-- Deferred Type Errors
-- check type at runtime
-- {-# OPTIONS_GHC -fdefer-type-errors #-}

-- Reader
-- The reader monad lets us access shared immutable state within a monadic context.
computation :: Reader MyContext (Maybe String)
computation = do
    n <- asks bar
    x <- asks foo
    if n > 0
        then return (Just x)
        else return Nothing

newtype Reader r a = Reader { runReader :: r -> a }
instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ runReader m . f

-- Wrtier
-- The writer monad lets us emit a lazy stream of values from within a monadic

example :: Writer [Int] String
example = do
    tell [1..5]
    tell [5..10]
    return "foo"

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Monoid w => Monad (Writer w) where
    return a = Writer (a, mempty)
    m >>= k = Writer $ let
        (a, w) = runWriter m
        (b, w') = runWriter (k a)
        in (b, w `mappend` w')

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

tell :: w -> Writer w ()
tell w = Writer ((), w)

-- This implementation is lazy so some care must be taken that one actually wants to only generate a stream of thunks. Most often the lazy writer is not suitable for use, instead implement the equivalent structure by embedding some monomial object inside a StateT monad, or using the strict version.

-- State Monad
-- The state monad allows functions within a stateful monadic context to access and modify shared state.

test :: State Int Int
test = do
    put 3
    modify (+1)
    get

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    State act >>= k = State $ \s ->
        let (a, s') = act s
        in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

-- Monad Transformer (mtl)
type State s = StateT s Identity
type Writer w = WriterT w Identity
type Reader r = ReaderT r Identity
instance Monad m => MonadState s (StateT s m)
instance Monad m => MonadReader r (ReaderT r m)
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m)

class MonadTrans t where
    lift :: Monad m => m a -> t m a
    -- lift . return === return
        -- lift (return x) === return x
    -- lift m >>= \x -> lift (f x) === lift $ m >>= \x -> f x
        -- lift (m >>= f) === lift m >>= (lift . f)
    
{- Although they are guaranteed to yield the same result, the operation of lifting the results between the monad levels is not without cost and crops up frequently when working with the monad traversal and looping functions. For example, all three of the functions on the left below are less efficient than the right hand side which performs the bind in the base monad instead of lifting on each iteration

Less Efficient         More Efficient
forever (lift m)    == lift (forever m)
mapM_ (lift . f) xs == lift (mapM_ f xs)
forM_ xs (lift . f) == lift (forM_ xs f)

-}


-- Monad (m :: * -> *)
-- MonadTrans (t :: (* -> *) -> * -> *)
    
-- ReaderT
-- Haskell 98 Reader
newtype Reader r a = Reader { runReader :: r -> a }

instance MonadReader r (Reader r) where
    ask = Reader id
    local f m = Reader (runReader m . f)

-- Transformer
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \_ -> m

-- MonadReader (MTL)
class (Monad m) => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance (Monad m) => MonadReader r (ReaderT r m) where
    ask = ReaderT return
    local f m = ReaderT $ \r -> runReaderT m (f r)

-- ask :: Reader r a
-- ask :: Monad m => ReaderT r m r
-- ask :: MonadReader r m => m r
-- In practice only the last one is used in modern Haskell.

-- Using -XGeneralizedNewtypeDeriving we can recover the functionality of instances of the underlying types composed in our transformer stack.
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- See WIW/VM.hs


-- Monad Morphisms
-- Hoist takes a monad morphism (a mapping a m a to a n a) and applies in on the inner value monad of a transformer stack, transforming the value under the outer layer.
hoist :: Monad m => (forall a. m a -> n a) -> t m b -> t n b
-- see WIW/VM.hs


-- Monomorphism Restriction
-- When the toplevel declarations of a module are generalized the monomorphism restricts that toplevel values (i.e. expressions not under a lambda ) whose type contains the subclass of the Num type from the Prelude are not generalized and instead are instantiated with a monotype tried sequentially from the list specified by the default which is normally Integer, then Double.
default (Integer, Double)

-- Extended Defaulting
-- Haskell normally applies several defaulting rules for ambigious literals in the absence of an explicit type signature. When an ambiguous literal is typechecked if at least one of its typeclass constraints is numeric and all of its classes are standard library classes, the module’s default list is consulted, and the first type from the list that will satisfy the context of the type variable is instantiated. So for instance given the following default rules.
-- default (C1 a,...,Cn a)
    -- The type variable a appears in no other constraints
    -- All the classes Ci are standard.
    -- At least one of the classes Ci is numeric.

-- The -XExtendedDefaultRules loosens the restriction that we’re constrained with working on Numerical typeclasses and the constraint that we can only work with standard library classes. If we’d like to have our string literals (using -XOverlodaedStrings) automatically default to the more efficient Text implementation instead of String we can twiddle the flag and GHC will perform the right substitution without the need for an explicit annotation on every string literal.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import qualified Data.Text as T

default (T.Text)

example = "foo"

-- For code typed at the GHCi prompt, the -XExtendedDefaultRules flag is always on, and cannot be switched off.

-- Safe Haskell
-- unsafe functions
-- unsafeCoerce :: a -> b
-- unsafePerformIO :: IO a -> a

{-# LANGUAGE Safe #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Safe #-}

import Unsafe.Coerce
import System.IO.Unsafe

bad1 :: String
bad1 = unsafePerformIO getLine

bad2 :: a
bad2 = unsafeCoerce 3.14 ()

-- Unsafe.Coerce: Can't be safely imported!
-- The module itself isn't safe.


-- Partial Type Signatures
-- hole type to dump inferred type
-- When the flag -XPartialTypeSignature is passed to GHC and the inferred type is unambiguous, GHC will let us leave the holes in place and the compilation will proceed.

-- Recursive Do (Lazy do-notation)

-- Applicative Do
-- With ApplicativeDo this instead desugars into use of applicative combinators and a laxer Applicative constraint.

-- RecordWildCards
-- Record wild cards allow us to expand out the names of a record as variables scoped as the labels of the record implicitly. The extension can be used to extract variables names into a scope or to assign to variables in a record drawing, aligning the record’s labels with the variables in scope for the assignment. The syntax introduced is the {..} pattern selector
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text
data Example = Example
    { e1 :: Int
    , e2 :: Text
    , e3 :: Text
    } deriving (Show)

-- Extracting from a record using wildcards.
scope :: Example -> (Int, Text, Text)
scope Example {..} = (e1, e2, e3)
-- Assign to a record using wildcards.
assign :: Example
assign = Example {..}
    where
        (e1, e2, e3) = (1, "Kirk", "Picard")

-- PatternSynonyms
-- see WIW/patternsynonyms.hs

-- OverloadedLabels
-- ref: http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/
-- implementation notes: https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Implementation
extract :: IsLabel "id" t => t
extract = #id

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

import GHC.Records (HasField(..))
import GHC.OverloadedLabels (IsLabel(..))

data S = MkS { foo :: Int }
data T x y z = forall b . MkT { foo :: y, bar :: b }

instance HasField x r a => IsLabel x (r -> a) where
    fromLabel = getField

main :: IO ()
main = do
    print (#foo (MkS 42))
    print (#foo (MkT True False))

-- MINIMAL
class Eq a where
    (==), (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
    {-# MINIMAL (==) #-}
    {-# MINIMAL (/=) #-}
    -- {-# MINIMAL (==) | (/=) #-} -- Either (==) or (/=)
    -- {-# MINIMAL (==) , (/=) #-} -- Both (==) and (/=)

-- FlexibleInstance
-- Without flexible instances, all instance heads must be type variable. The following would be legal.
instance MyClass (Maybe a)

-- With flexible instances, typeclass heads can be arbitrary nested types. The following would be forbidden without it.
instance MyClass (Maybe Int)

-- FlexibleContexts
-- Without flexible contexts, all contexts must be type variable. The following would be legal.
instance (MyClass a) => MyClass (Either a b)
-- With flexible contexts, typeclass contexts can be arbitrary nested types. The following would be forbidden without it.
instance (MyClass (Maybe a)) => MyClass (Either a b)

-- OverlappingInstances
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class MyClass a b where
    fn :: (a,b)

instance MyClass Int b where
    fn = error "b"

instance MyClass a Int where
    fn = error "a"

instance MyClass Int Int where
    fn = error "c"
-- Or
instance {-# OVERLAPPING #-} MyClass Int Int where
    fn = error "c"

example :: (Int, Int)
example = fn

-- IncoherentInstances
-- Incoherent instance loosens the restriction that there be only one specific instance, will choose one arbitrarily (based on the arbitrary sorting of it’s internal representation) and the resulting program will typecheck. This is generally pretty crazy and usually a sign of poor design.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class MyClass a b where
    fn :: (a,b)

instance MyClass Int b where
    fn = error "a"

instance MyClass a Int where
    fn = error "b"
-- Or
instance {-# INCOHERENT #-} MyClass a Int where
    fn = error "specific"


example :: (Int, Int)
example = fn

-- TypeSynonymInstances
-- Automatically derived for alias
-- instance MyClass OriginalType -> instance MyClass AliasType

-- BangPatterns
-- force evaluation to WBNF before pattern matching
($!) :: (a -> b) -> a -> b
f $! x = let !vx = x in f vx

-- Deepseq
class NFData a where
    rnf :: a -> ()
    rnf a = a `seq` ()

deepseq :: NFData a => a -> b -> a
deepseq a b = rnf a `seq` b

($!!) :: (NFData a) => (a -> b) -> a -> b
f $!! x = x `deepseq` f x

instance NFData Int

instance NFData (a -> b)

instance NFData a => NFData (Maybe a) where
    rnf Nothing = ()
    rnf (Just x) = rnf x

instance NFData a => NFData [a] where
    rnf [] = ()
    rnf (x:xs) = rnf x `seq` rnf xs

[1, undefined] `seq` ()
-- ()
[1, undefined] `deepseq` ()
-- Prelude.undefined

force :: NFData a => a
force x = x `deepseq` x

-- Irrefutable Patterns
g ~(a,b) = const 1 a
-- g undefined = 1

-- {-# LANGUAGE NoImplicitPrelude #-}
{-
module Custom (
    module Exports
) where

import Data.Int as Exports
import Data.Tuple as Exports
import Data.Maybe as Exports
import Data.String as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports
import Control.Monad.Trans.Except
    as Exports
    (ExceptT(ExceptT), Except, except, runExcept, runExceptT,
    mapExcept, mapExceptT, withExcept, withExceptT)

-}

-- Strings
{-

Variant           Module
strict text       Data.Text
lazy text         Data.Text.Lazy
strict bytestring Data.ByteString
lazy bytestring   Data.ByteString.Lazy

                     Data.Text       Data.Text.Lazy      Data.ByteString    Data.ByteString.Lazy
Data.Text            id              fromStrict          encodeUtf8         encodeUtf8
Data.Text.Lazy       toStrict        id                  encodeUtf8         encodeUtf8
Data.ByteString      decodeUtf8      decodeUtf8          id                 fromStrict
Data.ByteString.Lazy decodeUtf8      decodeUtf8          toStrict           id

-}

-- Overloaded... (replace default types)
-- Derived... (deriving typeclasses)

