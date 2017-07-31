-- Kind Polymorphism

-- The regular value level function which takes a function and applies it to an argument is universally generalized over in the usual Hindley-Milner way.

app :: forall a b. (a -> b) -> a -> b
app f a = f a

-- But when we do the same thing at the type-level we see we lose information about the polymorphism of the constructor applied.

-- TApp :: (* -> *) -> * -> *
data TApp f a = MkTApp (f a)

-- Turning on -XPolyKinds allows polymorphic variables at the kind level as well.

-- Default:   (* -> *) -> * -> *
-- PolyKinds: (k -> *) -> k -> *
data TApp f a = MkTApp (f a)

-- Default:   ((* -> *) -> (* -> *)) -> (* -> *)
-- PolyKinds: ((k -> *) -> (k -> *)) -> (k -> *)
data Mu f a = Roll (f (Mu f) a)

-- Default:   * -> *
-- PolyKinds: k -> *
data Proxy a = Proxy

-- Using the polykinded Proxy type allows us to write down type class functions over constructors of arbitrary kind arity.

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

data Proxy a = Proxy
data Rep = Rep

class PolyClass a where
  foo :: Proxy a -> Rep
  foo = const Rep

-- () :: *
-- [] :: * -> *
-- Either :: * -> * -> *

instance PolyClass ()
instance PolyClass []   -- poly kinds enables to put into * -> * to PolyClass a
instance PolyClass Either

-- Note that the datatype Proxy has kind forall k. k -> * (inferred by GHC), and the new PolyClass class has kind forall k. k -> Constraint.


-- For example we can write down the polymorphic S K combinators at the type level now.

{-# LANGUAGE PolyKinds #-}

newtype I (a :: *) = I a
newtype K (a :: *) (b :: k) = K a
newtype Flip (f :: k1 -> k2 -> *) (x :: k2) (y :: k1) = Flip (f y x)

unI :: I a -> a
unI (I x) = x

unK :: K a b -> a
unK (K x) = x

unFlip :: Flip f x y -> f y x
unFlip (Flip x) = x

-- Data Kinds

-- DataKinds are relatively simple. You turn on the extension and all of a sudden some of your data types have unique kinds! (Otherwise they are just constructors functions with type instead of kinds)

{-
    λ :set -XDataKinds
    λ data Animal = Pegasus | Octopus
    λ :kind Pegasus
    Pegasus :: Animal
-}
-- The -XDataKinds extension allows us to use refer to constructors at the value level and the type level. Consider a simple sum type:

data S a b = L a | R b

-- S :: * -> * -> *
-- L :: a -> S a b
-- R :: b -> S a b

-- With the extension enabled we see that our type constructors are now automatically promoted so that L or R can be viewed as both a data constructor of the type S or as the type L with kind S.

{-# LANGUAGE DataKinds #-}

data S a b = L a | R b

-- S :: * -> * -> *
-- L :: * -> S * *
-- R :: * -> S * *

-- Promoted data constructors can referred to in type signatures by prefixing them with a single quote. Also of importance is that these promoted constructors are not exported with a module by default, but type synonym instances can be created for the ticked promoted types and exported directly.

data Foo = Bar | Baz
type Bar = 'Bar
type Baz = 'Baz

-- Combining this with type families we see we can write meaningful, meaningful type-level functions by lifting types to the kind level.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

import Prelude hiding (Bool(..))

data Bool = False | True

type family Not (a :: Bool) :: Bool

type instance Not True = False
type instance Not False = True

false :: Not True ~ False => a
false = undefined

true :: Not False ~ True => a
true = undefined

-- Fails at compile time.
-- Couldn't match type 'False with 'True
invalid :: Not True ~ True => a
invalid = undefined