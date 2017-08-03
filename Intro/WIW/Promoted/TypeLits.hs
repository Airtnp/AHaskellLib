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

-- The variety of functions we can now write down are rather remarkable, allowing us to write meaningful logic at the type level.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

-- Type-level functions over type-level lists.

type family Reverse (xs :: [k]) :: [k] where
  Reverse '[] = '[]
  Reverse xs = Rev xs '[]

type family Rev (xs :: [k]) (ys :: [k]) :: [k] where
  Rev '[] i = i
  Rev (x ': xs) i = Rev xs (x ': i)

type family Length (as :: [k]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

type family If (p :: Bool) (a :: k) (b :: k) :: k where
  If True a b = a
  If False a b = b

type family Concat (as :: [k]) (bs :: [k]) :: [k] where
  Concat a '[] = a
  Concat '[] b = b
  Concat (a ': as) bs = a ': Concat as bs

type family Map (f :: a -> b) (as :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Sum (xs :: [Nat]) :: Nat where
  Sum '[] = 0
  Sum (x ': xs) = x + Sum xs

ex1 :: Reverse [1,2,3] ~ [3,2,1] => Proxy a
ex1 = Proxy

ex2 :: Length [1,2,3] ~ 3 => Proxy a
ex2 = Proxy

ex3 :: (Length [1,2,3]) ~ (Length (Reverse [1,2,3])) => Proxy a
ex3 = Proxy

-- Reflecting type level computations back to the value level.
ex4 :: Integer
ex4 = natVal (Proxy :: Proxy (Length (Concat [1,2,3] [4,5,6])))
-- 6

ex5 :: Integer
ex5 = natVal (Proxy :: Proxy (Sum [1,2,3]))
-- 6

-- Couldn't match type ‘2’ with ‘1’
ex6 :: Reverse [1,2,3] ~ [3,1,2] => Proxy a
ex6 = Proxy

type family Elem (a :: k) (bs :: [k]) :: Constraint where
  Elem a (a ': bs) = (() :: Constraint)
  Elem a (b ': bs) = a `Elem` bs

type family Sum (ns :: [Nat]) :: Nat where
  Sum '[] = 0
  Sum (n ': ns) = n + Sum ns

-- Kind Indexed Type Families

type family (a :: k) == (b :: k) :: Bool
type instance a == b = EqStar a b
type instance a == b = EqArrow a b
type instance a == b = EqBool a b

type family EqStar (a :: *) (b :: *) where
  EqStar a a = True
  EqStar a b = False

type family EqArrow (a :: k1 -> k2) (b :: k1 -> k2) where
  EqArrow a a = True
  EqArrow a b = False

type family EqBool a b where
  EqBool True  True  = True
  EqBool False False = True
  EqBool a     b     = False

type family EqList a b where
  EqList '[]        '[]        = True
  EqList (h1 ': t1) (h2 ': t2) = (h1 == h2) && (t1 == t2)
  EqList a          b          = False

type family a && b where
  True && True = True
  a    && a    = False

-- Typelevel Dictionaries

-- Much of this discussion of promotion begs the question whether we can create data structures at the type-level to store information at compile-time. For example a type-level association list can be used to model a map between type-level symbols and any other promotable types. Together with type-families we can write down type-level traversal and lookup functions.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

type family If (p :: Bool) (a :: k) (b :: k) :: k where
  If True a b = a
  If False a b = b

type family Lookup (k :: a) (ls :: [(a, b)]) :: Maybe b where
  Lookup k '[] = 'Nothing
  Lookup k ('(a, b) ': xs) = If (a == k) ('Just b) (Lookup k xs)

type M = [
    '("a", 1)
  , '("b", 2)
  , '("c", 3)
  , '("d", 4)
  ]

type K = "a"
type (!!) m (k :: Symbol) a = (Lookup k m) ~ Just a

value :: Integer
value = natVal ( Proxy :: (M !! "a") a => Proxy a )

{-

If we ask GHC to expand out the type signature we can view the explicit implementation of the type-level map lookup function.

(!!)
  :: If
       (GHC.TypeLits.EqSymbol "a" k)
       ('Just 1)
       (If
          (GHC.TypeLits.EqSymbol "b" k)
          ('Just 2)
          (If
             (GHC.TypeLits.EqSymbol "c" k)
             ('Just 3)
             (If (GHC.TypeLits.EqSymbol "d" k) ('Just 4) 'Nothing)))
     ~ 'Just v =>
     Proxy k -> Proxy v

-}