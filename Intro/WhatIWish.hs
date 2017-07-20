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

-- Applicative
-- Alternative

-- Arrows
class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
    id = Prelude.id
    (.) = (Prelude..)

(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f

class Category a => Arrow a where
    arr :: (b -> c) -> a b c
    first :: a b c -> a (b,d) (c,d)
    second :: a b c -> a (d,b) (d,c)
    (***) :: a b c -> a b' c' -> a (b,b') (c,c')
    (&&&) :: a b c -> a b c' -> a b (c,c')

-- See WIW/Circuit_and_Arrow
-- ref: https://en.wikibooks.org/wiki/Haskell/Understanding_arrows

class Arrow y => ArrowChoice y where
    -- Minimal implementation: left
    left  :: y a b -> y (Either a c) (Either b c)          -- maps over left choice
    right :: y a b -> y (Either c a) (Either c b)          -- maps over right choice
    (+++) :: y a c -> y b d -> y (Either a b) (Either c d) -- left and right combined
    (|||) :: y a c -> y b c -> y (Either a b) c            -- (+++), then merge results

class Arrow y => ArrowApply y where
    app :: y (y a b, a) b -- applies first component to second

instance Arrow (->) where
    arr f = f
    first f = f *** id
    second f = id *** f
    (***) f g ~(x,y) = (f x, g y)

-- Bifunctor (canonically 2-tuple)
class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    first :: (a -> b) -> p a c -> p b c
    second :: (b -> c) -> p a b -> p a c

-- Polyvariadic Functions

{-# LANGUAGE FlexibleInstances #-}
class Arg a where
    collect' :: [String] -> a

-- extract to IO
instance Arg (IO ()) where
    collect' acc = mapM_ putStrLn acc

-- extract to [String]
instance Arg [String] where
    collect' acc = acc

instance (Show a, Arg r) => Arg (a -> r) where
    collect' acc = \x -> collect' (acc ++ [show x])

collect :: Arg t => t
collect = collect' []

-- t is derived to Char -> Int -> Int
example :: [String]
example = collect 'a' 2 3

-- Exception
-- Control.Exception
-- Control.Monad.Catch
-- ExceptT (Control.Monad.Except)
-- Control.Spoon


-- RWS 
runReader :: Reader r a -> r -> a
runWriter :: Writer w a -> (a, w)
runState :: State s a -> s -> (a, s)

runRWS :: RWS r w s a -> r -> s -> (a, s, w)
execRWS :: RWS r w s a -> r -> s -> (s, w)
evalRWS :: RWS r w s a -> r -> s -> (a, w)

-- Cont
runCont :: Cont r a -> (a -> r) -> r
callCC :: MonadCont m => ((a -> m b) -> m a) -> m a
cont :: ((a -> r) -> r) -> Cont r a

newtype Cont r a = Cont { runCont :: ((a -> r) -> r) }

instance Monad (Cont r) where
    return a = Cont $ \k -> k a
    (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k)

class (Monad m) => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (Cont r) where
    callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k


-- MonadPlus
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

instance MonadPlus Maybe where
    mzero = Nothing
    Nothing `mplus` ys = ys
    xs `mplus` _ys = xs
    Nothing `mplus` Nothing = Nothing

-- MonadPlus is a monoid
-- mzero `mplus` a = a
--- a `mplus` mzero = a
-- (a `mplus` b) `mplus` c = a `mplus` (b `mplus` c)

when :: (Monad m) => Bool -> m () -> m ()
when p s = if p then s else return ()

guard :: MonadPlus m => Bool -> m ()
guard True = return ()
guard False = mzero

msum :: MonadPlus m => [m a] -> m a
msum = foldr mplus mzero

-- MonadFix
-- Similar to rec in -XRecursiveDo
fix :: (a -> a) -> a
fix f = let x = f x in x

mfix :: (a -> m a) -> m a

class Monad m => MonadFix m where
    mfix :: (a -> m a) -> m a

instance MonadFix Maybe where
    mfix f = let a = f (unJust a) in a
        where 
            unJust (Just x) = x
            unJust Nothing = error "mfix Maybe: Nothing"


-- ST Monad
runST :: (forall s. ST s a) -> a
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()

-- Free Monad
Pure :: a -> Free f a
Free :: f (Free f a) -> Free f a

liftF :: (Functor f, MonadFree f m) => f a -> m a
retract :: Monad f => Free f a -> f a
-- Free monads are monads which instead of having a join operation that combines computations, instead forms composite computations from application of a functor.
join :: Monad m => m (m a) -> m a
wrap :: MonadFree f m => f (m a) -> m a

-- Indexed Monads
-- Indexed monads are a generalisation of monads that adds an additional type parameter to the class that carries information about the computation or structure of the monadic implementation

class IxMonad md where
    return :: a -> md i i a
    (>>=) :: md i m a -> (a -> md m o b) -> md i o b

-- lifted-base
-- monad-base
-- monad-control


-- Quantification

-- Universal Quantification
-- Universal quantification the primary mechanism of encoding polymorphism in Haskell. The essence of universal quantification is that we can express functions which operate the same way for a set of types and whose function behavior is entirely determined only by the behavior of all types in this span.
-- {-# ExplicitForAll #-}

-- Free theorems
-- A universally quantified type-variable actually implies quite a few rather deep properties about the implementation of a function that can be deduced from its type signature. For instance the identity function in Haskell is guaranteed to only have one implementation since the only information that the information that can present in the body

-- Type System

-- Hindley-Milner type system
{-

e : x
    | λx:t.e -- value abstraction
    | e1 e2 -- application
    | let x = e1 in e2 -- let

t : t -> t -- function types
    | a -- type variables

σ : ∀ a . t -- type scheme

-}

-- In an implementation, the function generalize converts all type variables within the type into polymorphic type variables yielding a type scheme. Thefunction instantiate maps a scheme to a type, but with any polymorphic variables converted into unbound type variables.

-- Rank-N Types
-- System-F is the type system that underlies Haskell. System-F subsumes the HM type system in the sense that every type expressible in HM can be expressed within System-F. System-F is sometimes referred to in texts as the Girald-Reynolds polymorphic lambda calculus or second-order lambda calculus.
{-

t : t -> t -- function types
    | a -- type variables
    | λ a . t -- forall

e : x -- variables
    | λ(x:t).e -- value abstraction
    | e1 e2 -- value application
    | Λa.e -- type abstraction
    | e_t -- type application

An example with equivalents of GHC Core in comments:
-- ref: https://stackoverflow.com/questions/6121146/reading-ghc-core


id : ∀ t. t -> t
id = Λt. λx:t. x
-- id :: forall t. t -> t
-- id = \ (@ t) (x :: t) -> x

tr : ∀ a. ∀ b. a -> b -> a
tr = Λa. Λb. λx:a. λy:b. x
-- tr :: forall a b. a -> b -> a
-- tr = \ (@ a) (@ b) (x :: a) (y :: b) -> x

fl : ∀ a. ∀ b. a -> b -> b
fl = Λa. Λb. λx:a. λy:b. y
-- fl :: forall a b. a -> b -> b

nil : ∀ a. [a]
nil = Λa. Λb. λz:b. λf:(a -> b -> b). z
-- nil :: forall a. [a]
-- nil = \ (@ a) (@ b) (z :: b) (f :: a -> b -> b) -> z

cons : ∀ a. a -> [a] -> [a]
cons = Λa. λx:a. λxs:(∀ b. b -> (a -> b -> b) -> b).
    Λb. λz:b. λf : (a -> b -> b). f x (xs_b z f)
-- cons :: forall a. a -> [a] -> [a]
-- cons = \ (@ a) (x :: a) (xs :: forall b. b -> (a -> b -> b) -> b)
--     (@ b) (z :: b) (f :: a -> b -> b) -> f x (xs @ b z f)

-}


{-# LANGUAGE RankNTypes #-}
-- Can't unify ( Bool ~ Char )
rank1 :: forall a. (a -> a) -> (Bool, Char)
rank1 f = (f True, f 'a')

rank2 :: (forall a. a -> a) -> (Bool, Char)
rank2 f = (f True, f 'a')

auto :: (forall a. a -> a) -> (forall b. b -> b)
auto x = x

xauto :: forall a. (forall b. b -> b) -> a -> a
xauto f = f

-- Monomorphic Rank 0: t
-- Polymorphic Rank 1: forall a. a -> t
-- Polymorphic Rank 2: (forall a. a -> t) -> t
-- Polymorphic Rank 3: ((forall a. a -> t) -> t) -> t


-- Existential Quantification
-- An existential type is a pair of a type and a term with a special set of packing and unpacking semantics. The type of the value encoded in the existential is known by the producer but not by the consumer of the existential value.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- ∃ t. (t, t → t, t → String)
data Box = forall a. Box a (a -> a) (a -> String)

boxa :: Box
boxa = Box 1 negate show

boxb :: Box
boxb = Box "foo" reverse show

apply :: Box -> String
apply (Box x f p) = p (f x)

-- ∃ t. Show t => t
data SBox = forall a. Show a => SBox a

boxes :: [SBox]
boxes = [SBox (), SBox 2, SBox "foo"]

showBox :: SBox -> String
showBox (SBox a) = show a

main :: IO ()
main = mapM_ (putStrLn . showBox) boxes
-- ()
-- 2
-- "foo"

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

-- a b are existentially bound type variables, m is a free type variable
data MonadI m = MonadI
  { _return :: forall a . a -> m a
  , _bind   :: forall a b . m a -> (a -> m b) -> m b
  }

monadMaybe:: MonadI Maybe
monadMaybe = MonadI
  { _return = Just
  , _bind   = \m f -> case m of
      Nothing -> Nothing
      Just x  -> f x
  }


-- Impredicative Types
-- Although extremely brittle, GHC also has limited support for impredicative polymorphism which allows instantiating type variable with a polymorphic type. Implied is that this loosens the restriction that quantifiers must precede arrow types and now they may be placed inside of type-constructors.

-- Can't unify ( Int ~ Char )
revUni :: forall a. Maybe ([a] -> [a]) -> Maybe ([Int], [Char])
revUni (Just g) = Just (g [3], g "hello")
revUni Nothing = Nothing

{-# LANGUAGE ImpredicativeTypes #-}

-- Uses higher-ranked polymorphism.
f :: (forall a. [a] -> a) -> (Int, Char)
f get = (get [1,2], get ['a', 'b', 'c'])

-- Uses impredicative polymorphism.
g :: Maybe (forall a. [a] -> a) -> (Int, Char)
g Nothing = (0, '0')
g (Just get) = (get [1,2], get ['a','b','c'])