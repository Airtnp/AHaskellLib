-- Singleton
{-

A singleton type is a type with a single value inhabitant. Singleton types can be constructed in a variety of ways using GADTs or with data families.

data instance Sing (a :: Nat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

data instance Sing (a :: Maybe k) where
  SNothing :: Sing 'Nothing
  SJust :: Sing x -> Sing ('Just x)

data instance Sing (a :: Bool) where
  STrue :: Sing True
  SFalse :: Sing False

Promoted Naturals
Value-level	   Type-level	        Models
SZ	           Sing 'Z	            0
SS SZ          Sing ('S 'Z)	        1
SS (SS SZ)	   Sing ('S ('S 'Z))	2

Promoted Booleans
Value-level	   Type-level	        Models
SFalse	       Sing 'False	        False
STrue	       Sing 'True	        True

Promoted Maybe
Value-level	   Type-level	        Models
SJust a	       Sing (SJust 'a)	    Just a
SNothing	   Sing Nothing	        Nothing

Singleton types are an integral part of the small cottage industry of faking dependent types in Haskell, i.e. constructing types with terms predicated upon values. Singleton types are a way of "cheating" by modeling the map between types and values as a structural property of the type.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Proxy
import GHC.Exts (Any)
import Prelude hiding (succ)

data Nat = Z | S Nat

-- kind-indexed data family
data family Sing (a :: k)

data instance Sing (a :: Nat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

data instance Sing (a :: Maybe k) where
  SNothing :: Sing 'Nothing
  SJust :: Sing x -> Sing ('Just x)

data instance Sing (a :: Bool) where
  STrue :: Sing True
  SFalse :: Sing False

data Fin (n :: Nat) where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

data Vec a n where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

class SingI (a :: k) where
  sing :: Sing a

instance SingI Z where
  sing = SZ

instance SingI n => SingI (S n) where
  sing = SS sing

deriving instance Show Nat
deriving instance Show (SNat a)
deriving instance Show (SBool a)
deriving instance Show (Fin a)
deriving instance Show a => Show (Vec a n)

type family (m :: Nat) :+ (n :: Nat) :: Nat where
  Z :+ n = n
  S m :+ n = S (m :+ n)

type SNat (k :: Nat) = Sing k
type SBool (k :: Bool) = Sing k
type SMaybe (b :: a) (k :: Maybe a) = Sing k

size :: Vec a n -> SNat n
size Nil         = SZ
size (Cons x xs) = SS (size xs)

forget :: SNat n -> Nat
forget SZ = Z
forget (SS n) = S (forget n)

natToInt :: Integral n => Nat -> n
natToInt Z     = 0
natToInt (S n) = natToInt n + 1

intToNat :: (Integral a, Ord a) => a -> Nat
intToNat 0 = Z
intToNat n = S $ intToNat (n - 1)

sNatToInt :: Num n => SNat x -> n
sNatToInt SZ     = 0
sNatToInt (SS n) = sNatToInt n + 1

index :: Fin n -> Vec a n -> a
index FZ (Cons x _)      = x
index (FS n) (Cons _ xs) = index n xs


test1 :: Fin (S (S (S Z)))
test1 = FS (FS FZ)

test2 :: Int
test2 = index FZ (1 `Cons` (2 `Cons` Nil))

test3 :: Sing ('Just ('S ('S Z)))
test3 = SJust (SS (SS SZ))

test4 :: Sing ('S ('S Z))
test4 = SS (SS SZ)

-- polymorphic constructor SingI
test5 :: Sing ('S ('S Z))
test5 = sing

-- The builtin singleton types provided in GHC.TypeLits have the useful implementation that type-level values can be reflected to the value-level and back up to the type-level, albeit under an existential.

someNatVal :: Integer -> Maybe SomeNat
someSymbolVal :: String -> SomeSymbol

natVal :: KnownNat n => proxy n -> Integer
symbolVal :: KnownSymbol n => proxy n -> String
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Proxy
import GHC.TypeLits

a :: Integer
a = natVal (Proxy :: Proxy 1)
-- 1

b :: String
b = symbolVal (Proxy :: Proxy "foo")
-- "foo"

c :: Integer
c = natVal (Proxy :: Proxy (2 + 3))
-- 5