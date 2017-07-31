-- TypeLevel Literals

-- Numbers
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

data Vec :: Nat -> * -> * where
  Nil :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (1 + n) a

-- GHC 7.6 will not reduce
-- vec3 :: Vec (1 + (1 + (1 + 0))) Int

vec3 :: Vec 3 Int
vec3 = 0 `Cons` (1 `Cons` (2 `Cons` Nil))
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

import GHC.TypeLits
import Data.Type.Equality

data Foo :: Nat -> * where
  Small    :: (n <= 2)  => Foo n
  Big      :: (3 <= n) => Foo n

  Empty    :: ((n == 0) ~ True) => Foo n
  NonEmpty :: ((n == 0) ~ False) => Foo n

big :: Foo 10
big = Big

small :: Foo 2
small = Small

empty :: Foo 0
empty = Empty

nonempty :: Foo 3
nonempty = NonEmpty

-- String
data ErrorMessage where
  Text :: Symbol -> ErrorMessage
  ShowType :: t -> ErrorMessage

  -- Put two messages next to each other
  (:<>:) :: ErrorMessage -> ErrorMessage -> ErrorMessage

  -- Put two messages on top of each other
  (:$$:) :: ErrorMessage -> ErrorMessage -> ErrorMessage

example.hs:1:1: error:
    • My custom error message line 1.
    • My custom error message line 2.
    • In the expression: example
      In an equation for ‘foo’: foo = ECoerce (EFloat 3) (EInt 4)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits

instance
  -- Error Message
  TypeError (Text "Equality is not defined for functions"
  :$$:
  (ShowType a :<>: Text " -> " :<>: ShowType b))

  -- Instance head
  => Eq (a -> b) where (==) = undefined

-- Fail when we try to equate two functions
example = id == id

-- A less contrived example would be creating a type-safe embedded DSL that enforces invariants about the semantics at the type-level. We've been able to do this sort of thing using GADTs and type-families for a while but the error reporting has been horrible. With 8.0 we can have type-families that emit useful type errors that reflect what actually goes wrong and integrate this inside of GHC.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits

type family Coerce a b where
  Coerce Int Int     = Int
  Coerce Float Float = Float
  Coerce Int Float   = Float
  Coerce Float Int   = TypeError (Text "Cannot cast to smaller type")

data Expr a where
  EInt    :: Int -> Expr Int
  EFloat  :: Float -> Expr Float
  ECoerce :: Expr b -> Expr c -> Expr (Coerce b c)

foo :: Expr Int
foo = ECoerce (EFloat 3) (EInt 4)

-- Type Equality

-- Continuing with the theme of building more elaborate proofs in Haskell, GHC 7.8 recently shipped with the Data.Type.Equality module which provides us with an extended set of type-level operations for expressing the equality of types as values, constraints, and promoted booleans.

(~)   :: k -> k -> Constraint
(==)  :: k -> k -> Bool
(<=)  :: Nat -> Nat -> Constraint
(<=?) :: Nat -> Nat -> Bool
(+)   :: Nat -> Nat -> Nat
(-)   :: Nat -> Nat -> Nat
(*)   :: Nat -> Nat -> Nat
(^)   :: Nat -> Nat -> Nat
(:~:)     :: k -> k -> *

Refl      :: a1 :~: a1
sym       :: (a :~: b) -> b :~: a
trans     :: (a :~: b) -> (b :~: c) -> a :~: c
castWith  :: (a :~: b) -> a -> b
gcastWith :: (a :~: b) -> (a ~ b => r) -> r

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.TypeLits
import Data.Type.Equality

type Not a b = ((b == a) ~ False)

restrictUnit :: Not () a => a -> a
restrictUnit = id

-- Data.Type.Equality.EqStar a Char ~ 'False => a -> a
-- can also receive Char -> Char
restrictChar :: Not Char a => a -> a
restrictChar = id
