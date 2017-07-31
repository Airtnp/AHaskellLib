-- Constraint Kinds
-- GHC's implementation also exposes the predicates that bound quantifiers in Haskell as types themselves, with the -XConstraintKinds extension enabled. Using this extension we work with constraints as first class types.

Num :: * -> Constraint
Odd :: * -> Constraint

type T1 a = (Num a, Ord a)

-- The empty constraint set is indicated by () :: Constraint.

-- For a contrived example if we wanted to create a generic Sized class that carried with it constraints on the elements of the container in question we could achieve this quite simply using type families.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.Exts (Constraint)
import Data.Hashable
import Data.HashSet

type family Con a :: Constraint
type instance Con [a] = (Ord a, Eq a)
type instance Con (HashSet a) = (Hashable a)

class Sized a where
  gsize :: Con a => a -> Int

instance Sized [a] where
  gsize = length

instance Sized (HashSet a) where
  gsize = size

-- One use-case of this is to capture the typeclass dictionary constrained by a function and reify it as a value.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

import GHC.Exts (Constraint)

data Dict :: Constraint -> * where
  Dict :: (c) => Dict c

dShow :: Dict (Show a) -> a -> String
dShow Dict x = show x

dEqNum :: Dict (Eq a, Num a) -> a -> Bool
dEqNum Dict x = x == 0


fShow :: String
fShow = dShow Dict 10

fEqual :: Bool
fEqual = dEqNum Dict 0