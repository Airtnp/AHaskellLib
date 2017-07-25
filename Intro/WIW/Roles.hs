-- Roles
{-

Roles are a further level of specification for type variables parameters of datatypes.

    nominal
    representational
    phantom

They were added to the language to address a rather nasty and long-standing bug around the correspondence between a newtype and its runtime representation. The fundamental distinction that roles introduce is there are two notions of type equality. Two types are nominally equal when they have the same name. This is the usual equality in Haskell or Core. Two types are representationally equal when they have the same representation. (If a type is higher-kinded, all nominally equal instantiations lead to representationally equal types.)

    nominal - Two types are the same.
    representational - Two types have the same runtime representation.

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Age = MkAge { unAge :: Int }

type family Inspect x
type instance Inspect Age = Int
type instance Inspect Int = Bool

class Boom a where
  boom :: a -> Inspect a

instance Boom Int where
  boom = (== 0)

deriving instance Boom Age

-- GHC 7.6.3 exhibits undefined behavior
failure = boom (MkAge 3)
-- -6341068275333450897

-- Roles are normally inferred automatically, but with the RoleAnnotations extension they can be manually annotated. Except in rare cases this should not be necessary although it is helpful to know what is going on under the hood.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

data Nat = Zero | Suc Nat

type role Vec nominal representational
data Vec :: Nat -> * -> * where
  Nil  :: Vec Zero a
  (:*) :: a -> Vec n a -> Vec (Suc n) a

type role App representational nominal
data App (f :: k -> *) (a :: k) = App (f a)

type role Mu nominal nominal
data Mu (f :: (k -> *) -> k -> *) (a :: k) = Roll (f (Mu f) a)

type role Proxy phantom
data Proxy (a :: k) = Proxy
coerce :: Coercible * a b => a -> b
class (~R#) k k a b => Coercible k a b