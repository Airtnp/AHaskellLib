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

-- Scoped Type Variables
-- Normally the type variables used within the toplevel signature for a function are only scoped to the type-signature and not the body of the function and its rigid signatures over terms and let/where clauses. Enabling -XScopedTypeVariables loosens this restriction allowing the type variables mentioned in the toplevel to be scoped within the value-level body of a function and all signatures contained therein.

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

poly :: forall a b c. a -> b -> c -> (a, a)
poly x y z = (f x y, f x z)
    where
    -- second argument is universally quantified from inference
    -- f :: forall t0 t1. t0 -> t1 -> t0
    f x' _ = x'

mono :: forall a b c. a -> b -> c -> (a, a)
mono x y z = (f x y, f x z)
    where
    -- b is not implictly universally quantified because it is in scope
    f :: a -> b -> a
    f x' _ = x'

-- ~: Equality constraints. Assert that two types in a context must be the same:
-- example :: F a ~ b => a -> b
-- Here the type "F a" must be the same as the type "b", which allows one to constrain polymorphism (especially where type families are involved), but to a lesser extent than functional dependencies. See Type Families.
-- ref: http://blog.infinitenegativeutility.com/2017/1/haskell-type-equality-constraints

-- GADT
-- Generalized Algebraic Data types (GADTs) are an extension to algebraic datatypes that allow us to qualify the constructors to datatypes with type equality constraints, allowing a class of types that are not expressible using vanilla ADTs.

-- Vanilla
data List a
    = Empty
    | Cons a (List a)

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
-- GADTSyntax
data List a where
    Empty :: List a
    Cons :: a -> List a -> List a

-- For example
data Term a
    = Lit a
    | Succ (Term a)
    | IsZero (Term a)
-- we can't write
-- eval (Succ t) = 1 + eval t
-- also wrongly
-- failure = Succ ( Lit True )

-- Using a GADT we can express the type invariants for our language (i.e. only type-safe expressions are representable). Pattern matching on this GADTs then carries type equality constraints without the need for explicit tags.

data Term a where
    Lit :: a -> Term a
    Succ :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool
    If :: Term Bool -> Term a -> Term a -> Term a

eval :: Term a -> a
eval (Lit i) = i -- Term a
eval (Succ t) = 1 + eval t -- Term (a ~ Int)
eval (IsZero i) = eval i == 0 -- Term (a ~ Int)
eval (If b e1 e2) = if eval b then eval e1 else eval e2 -- Term (a ~ Bool)

-- This is rejected at compile-time.
-- failure = Succ ( Lit True )

-- Explicit equality constraints (a ~ b) can be added to a function’s context.
{-

f :: a -> a -> (a, a)
f :: (a ~ b) => a -> b -> (a,b)
(Int ~ Int) => ...
(a ~ Int) => ...
(Int ~ a) => ...
(a ~ b) => ...
(Int ~ Bool) => ... -- Will not typecheck.

This is effectively the implementation detail of what GHC is doing behind the scenes to implement GADTs ( implicitly passing and threading equality terms around ). If we wanted we could do the same setup that GHC does just using equality constraints and existential quantification. Indeed, the internal representation of GADTs is as regular algebraic datatypes that carry coercion evidence as arguments.

-- Using Constraints
data Exp a
    = (a ~ Int) => LitInt a
    | (a ~ Bool) => LitBool a
    | forall b. (b ~ Bool) => If (Exp b) (Exp a) (Exp a)

-- Using GADTs
-- data Exp a where
    -- LitInt :: Int -> Exp Int
    -- LitBool :: Bool -> Exp Bool
    -- If :: Exp Bool -> Exp a -> Exp a -> Exp a

eval :: Exp a -> a
eval e = case e of
    LitInt i -> i
    LitBool b -> b
    If b tr fl -> if eval b then eval tr else eval fl

data T :: * -> * where
    T1 :: Int -> T Int
    T2 :: T a

f (T1 n) = [n]
f T2 = []

-}

-- Kind Signatures
{-

κ : *
  | κ -> κ

Int :: *
Maybe :: * -> *
Either :: * -> * -> *

-}

-- With the KindSignatures extension enabled we can now annotate top level type signatures with their explicit kinds, bypassing the normal kind inference procedures.

{-# Language GADTs #-}
{-# LANGUAGE KindSignatures #-}

data Term a :: * where
  Lit    :: a -> Term a
  Succ   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a

data Vec :: * -> * -> * where
  Nil :: Vec n a
  Cons :: a -> Vec n a -> Vec n a

data Fix :: (* -> *) -> * where
  In :: f (Fix f) -> Fix f


-- Void
{-

Using a newtype wrapper we can create a type where recursion makes it impossible to construct an inhabitant.

-- Void :: Void -> Void
newtype Void = Void Void

Or using -XEmptyDataDecls we can also construct the uninhabited type equivalently as a data declaration with no constructors.

data Void
The only inhabitant of both of these types is a diverging term like (undefined).


-}

-- Type Inhabitation
-- ref: https://www.zhihu.com/question/41925054

-- Phantom Type


-- Interpreters

-- Lambda Calculus
-- A lambda expression in which all variables that appear in the body of the expression are referenced in an outer lambda binder is said to be closed while an expression with unbound free variables is open.

type Name = String

data Exp
    = Var Name
    | Lam Name Exp
    | App Exp Exp

-- HOAS
-- Higher Order Abstract Syntax (HOAS) is a technique for implementing the lambda calculus in a language where the binders of the lambda expression map directly onto lambda binders of the host language ( i.e. Haskell ) to give us substitution machinery in our custom language by exploiting Haskell's implementation.


-- Testing
-- Quickcheck

quickCheck :: Testable prop => prop -> IO ()
(==>) :: Testable prop => Bool -> prop -> Property
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
choose :: Random a => (a, a) -> Gen a

-- data generator:
-- class Arbitrary

-- SmallCheck/QuickCheck/Criterion/Tasty/silently


-- Type families
-- Resolution of vanilla Haskell 98 typeclasses proceeds via very simple context reduction that minimizes interdependency between predicates, resolves superclasses, and reduces the types to head normal form. For example:

-- (Eq [a], Ord [a]) => [a]
-- ==> Ord a => [a]

-- If a single parameter typeclass expresses a property of a type ( i.e. it's in a class or not in class ) then a multiparameter typeclass expresses relationships between types. For example if we wanted to express the relation a type can be converted to another type we might use a class like:

{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Char

class Convertible a b where
  convert :: a -> b

instance Convertible Int Integer where
  convert = toInteger

instance Convertible Int Char where
  convert = chr

instance Convertible Char Int where
  convert = ord

-- Of course now our instances for Convertible Int are not unique anymore, so there no longer exists a nice procedure for determining the inferred type of b from just a. To remedy this let's add a functional dependency a -> b, which tells GHC that an instance a uniquely determines the instance that b can be. So we'll see that our two instances relating Int to both Integer and Char conflict.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


import Data.Char

class Convertible a b | a -> b where
  convert :: a -> b

instance Convertible Int Char where
  convert = chr

instance Convertible Char Int where
  convert = ord

-- Functional dependencies conflict between instance declarations:
  -- instance Convertible Int Integer
  -- instance Convertible Int Char

-- Now there's a simpler procedure for determining instances uniquely and multiparameter typeclasses become more usable and inferable again. Effectively a functional dependency | a -> b says that we can't define multiple multiparamater typeclass instances with the same a but different b.

-- Now let's make things not so simple. Turning on UndecidableInstances loosens the constraint on context reduction that can only allow constraints of the class to become structural smaller than its head. As a result implicit computation can now occur within in the type class instance search. Combined with a type-level representation of Peano numbers we find that we can encode basic arithmetic at the type-level.

-- Injectivity
-- The type level functions defined by type-families are not necessarily injective, the function may map two distinct input types to the same output type. This differs from the behavior of type constructors ( which are also type-level functions ) which are injective.

data Maybe a = Nothing | Just a
-- Maybe a ~ Maybe b  implies  a ~ b

type instance F Int = Bool
type instance F Char = Bool

-- F a ~ F b does not imply  a ~ b, in general

-- NonEmpty
-- Rather than having degenerate (and often partial) cases of many of the Prelude functions to accommodate the null case of lists, it is sometimes preferable to statically enforce empty lists from even being constructed as an inhabitant of a type.

infixr 5 :|, <|
data NonEmpty a = a :| [a]

head :: NonEmpty a -> a
toList :: NonEmpty a -> [a]
fromList :: [a] -> NonEmpty a
head :: NonEmpty a -> a
head ~(a :| _) = a
import Data.List.NonEmpty
import Prelude hiding (head, tail, foldl1)
import Data.Foldable (foldl1)

a :: NonEmpty Integer
a = fromList [1,2,3]
-- 1 :| [2,3]

b :: NonEmpty Integer
b = 1 :| [2,3]
-- 1 :| [2,3]

c :: NonEmpty Integer
c = fromList []
-- *** Exception: NonEmpty.fromList: empty list

d :: Integer
d = foldl1 (+) $ fromList [1..100]
-- 5050

-- TypeFamilyDependencies

-- Type families historically have not been injective, i.e. they are not guaranteed to maps distinct elements of its arguments to the same element of its result. The syntax is similar to the multiparmater typeclass functional dependencies in that the resulting type is uniquely determined by a set of the type families parameters.

{-# LANGUAGE XTypeFamilyDependencies #-}

type family F a b c = (result :: k) | result -> a b c
type instance F Int  Char Bool = Bool
type instance F Char Bool Int  = Int
type instance F Bool Int  Char = Char



-- Promotion
-- Higher Kinded Types

{-

What are higher kinded types?

The kind system in Haskell is unique by contrast with most other languages in that it allows datatypes to be constructed which take types and type constructor to other types. Such a system is said to support higher kinded types.

All kind annotations in Haskell necessarily result in a kind * although any terms to the left may be higher-kinded (* -> *).

The common example is the Monad which has kind * -> *. But we have also seen this higher-kindedness in free monads.

-}

data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a

data Cofree f a where
  Cofree :: a -> f (Cofree f a) -> Cofree f a

Free :: (* -> *) -> * -> *
Cofree :: (* -> *) -> * -> *

-- For instance Cofree Maybe a for some monokinded type a models a non-empty list with Maybe :: * -> *.

-- Cofree Maybe a is a non-empty list
testCofree :: Cofree Maybe Int
testCofree = (Cofree 1 (Just (Cofree 2 Nothing)))

-- Generics

-- Typeable
-- The Typeable class be used to create runtime type information for arbitrary types.

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x
  | typeOf x == typeOf ret = Just ret
  | otherwise = Nothing
  where
    ret = unsafeCast x

-- Dynamic
-- Since we have a way of querying runtime type information we can use this machinery to implement a Dynamic type. This allows us to box up any monotype into a uniform type that can be passed to any function taking a Dynamic type which can then unpack the underlying value in a type-safe way.

toDyn :: Typeable a => a -> Dynamic
fromDyn :: Typeable a => Dynamic -> a -> a
fromDynamic :: Typeable a => Dynamic -> Maybe a
cast :: (Typeable a, Typeable b) => a -> Maybe b

-- Use of Dynamic is somewhat rare, except in odd cases that have to deal with foreign memory and FFI interfaces. Using it for business logic is considered a code smell. Consider a more idiomatic solution.


-- Syb
-- Using the interface provided by the Data we can retrieve the information we need to, at runtime, inspect the types of expressions and rewrite them, collect terms, and find subterms matching specific predicates.

everywhere :: (forall a. Data a => a -> a) -> forall a. Data a => a -> a
everywhereM :: Monad m => GenericM m -> GenericM m
somewhere :: MonadPlus m => GenericM m -> GenericM m
listify :: Typeable r => (r -> Bool) -> GenericQ [r]
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- For example consider we have some custom collection of datatypes for which we want to write generic transformations that transform numerical subexpressions according to set of rewrite rules. We can use syb to write the transformation rules quite succinctly.

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Typeable
import Data.Generics.Schemes
import Data.Generics.Aliases (mkT)

data MyTuple a = MyTuple a Float
  deriving (Data, Typeable, Show)

exampleT :: Data a => MyTuple a -> MyTuple a
exampleT = everywhere (mkT go1) . everywhere (mkT go2)
  where
    go1 :: Int -> Int
    go1 x = succ x

    go2 :: Float -> Float
    go2 x = succ x

findFloat :: Data x => x -> Maybe Float
findFloat = gfindtype

main :: IO ()
main = do
  let term = MyTuple (MyTuple (1 :: Int) 2.0) 3.0
  print (exampleT term)
  print (gsize term)
  print (findFloat term)
  print (listify ((>0) :: (Int -> Bool)) term)

