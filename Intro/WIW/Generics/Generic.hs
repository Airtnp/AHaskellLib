-- Generic
-- The most modern method of doing generic programming uses type families to achieve a better method of deriving the structural properties of arbitrary type classes. Generic implements a typeclass with an associated type Rep ( Representation ) together with a pair of functions that form a 2-sided inverse ( isomorphism ) for converting to and from the associated type and the derived type in question.

class Generic a where
  -- Encode the representation of a user datatype
  type Rep a :: * -> *
  -- Convert from the datatype to its representation
  from  :: a -> (Rep a) x
  -- Convert from the representation to the datatype
  to    :: (Rep a) x -> a

class Datatype d where
  datatypeName :: t d f a -> String
  moduleName :: t d f a -> String

class Constructor c where
  conName :: t c f a -> String


-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p

-- | Tag for M1: datatype
data D
-- | Tag for M1: constructor
data C

-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Type synonym for encoding meta-information for datatypes
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors
type C1 = M1 C

-- Using the deriving mechanics GHC can generate this Generic instance for us mechanically, if we were to write it by hand for a simple type it might look like this:

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Generics

data Animal
  = Dog
  | Cat

instance Generic Animal where
  type Rep Animal = D1 T_Animal ((C1 C_Dog U1) :+: (C1 C_Cat U1))

  from Dog = M1 (L1 (M1 U1))
  from Cat = M1 (R1 (M1 U1))

  to (M1 (L1 (M1 U1))) = Dog
  to (M1 (R1 (M1 U1))) = Cat

data T_Animal
data C_Dog
data C_Cat

instance Datatype T_Animal where
  datatypeName _ = "Animal"
  moduleName _ = "Main"

instance Constructor C_Dog where
  conName _ = "Dog"

instance Constructor C_Cat where
  conName _ = "Cat"

{-
Use kind! in GHCi we can look at the type family Rep associated with a Generic instance.

λ: :kind! Rep Animal
Rep Animal :: * -> *
= M1 D T_Animal (M1 C C_Dog U1 :+: M1 C C_Cat U1)

λ: :kind! Rep ()
Rep () :: * -> *
= M1 D GHC.Generics.D1() (M1 C GHC.Generics.C1_0() U1)

λ: :kind! Rep [()]
Rep [()] :: * -> *
= M1
    D
    GHC.Generics.D1[]
    (M1 C GHC.Generics.C1_0[] U1
     :+: M1
           C
           GHC.Generics.C1_1[]
           (M1 S NoSelector (K1 R ()) :*: M1 S NoSelector (K1 R [()])))


-}

-- Now the clever bit, instead writing our generic function over the datatype we instead write it over the Rep and then reify the result using from. So for an equivalent version of Haskell's default Eq that instead uses generic deriving we could write:


lass GEq' f where
  geq' :: f a -> f a -> Bool

instance GEq' U1 where
  geq' _ _ = True

instance (GEq c) => GEq' (K1 i c) where
  geq' (K1 a) (K1 b) = geq a b

instance (GEq' a) => GEq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

-- Equality for sums.
instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

-- Equality for products.
instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2

-- To accommodate the two methods of writing classes (generic-deriving or custom implementations) we can use the DefaultSignatures extension to allow the user to leave typeclass functions blank and defer to Generic or to define their own.

{-# LANGUAGE DefaultSignatures #-}

class GEq a where
  geq :: a -> a -> Bool

  default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
  geq x y = geq' (from x) (from y)

-- Now anyone using our library need only derive Generic and create an empty instance of our typeclass instance without writing any boilerplate for GEq.

-- Here is a complete example for deriving equality generics:

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

import GHC.Generics

-- Auxiliary class
class GEq' f where
  geq' :: f a -> f a -> Bool

instance GEq' U1 where
  geq' _ _ = True

instance (GEq c) => GEq' (K1 i c) where
  geq' (K1 a) (K1 b) = geq a b

instance (GEq' a) => GEq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2

--
class GEq a where
  geq :: a -> a -> Bool
  default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
  geq x y = geq' (from x) (from y)

-- Base equalities
instance GEq Char where geq = (==)
instance GEq Int where geq = (==)
instance GEq Float where geq = (==)

-- Equalities derived from structure of (:+:) and (:*:)
instance GEq a => GEq (Maybe a)
instance (GEq a, GEq b) => GEq (a,b)

main :: IO ()
main = do
  print $ geq 2 (3 :: Int)
  print $ geq 'a' 'b'
  print $ geq (Just 'a') (Just 'a')
  print $ geq ('a','b') ('a', 'b')

-- Generic Deriving

-- Using Generics many common libraries provide a mechanisms to derive common typeclass instances. Some real world examples:

{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Hashable

data Color = Red | Green | Blue deriving (Generic, Show)

instance Hashable Color where

example1 :: Int
example1 = hash Red
-- 839657738087498284

example2 :: Int
example2 = hashWithSalt 0xDEADBEEF Red
-- 62679985974121021

{-# LANGUAGE DeriveGeneric #-}

import Data.Word
import Data.ByteString
import Data.Serialize

import GHC.Generics

data Val = A [Val] | B [(Val, Val)] | C
  deriving (Generic, Show)

instance Serialize Val where

encoded :: ByteString
encoded = encode (A [B [(C, C)]])
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\STX"

bytes :: [Word8]
bytes = unpack encoded
-- [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,2,2]

decoded :: Either String Val
decoded = decode encoded

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import GHC.Generics

data Point = Point { _x :: Double, _y :: Double }
   deriving (Show, Generic)

instance FromJSON Point
instance ToJSON Point

example1 :: Maybe Point
example1 = decode "{\"x\":3.0,\"y\":-1.0}"

example2 = encode $ Point 123.4 20

-- Higher Kinded Generics

class Generic1 f where
  type Rep1 f :: * -> *
  from1  :: f a -> (Rep1 f) a
  to1    :: (Rep1 f) a -> f a

-- So for instance Maybe has Rep1 of the form:

type instance Rep1 Maybe
  = D1
      GHC.Generics.D1Maybe
      (C1 C1_0Maybe U1
       :+: C1 C1_1Maybe (S1 NoSelector Par1))

-- generics-sop


-- Generic Wiki

{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
 
import GHC.Generics
import Data.Bits
 
 
data Bit = O | I deriving Show
 
class Serialize a where
  put :: a -> [Bit]
 
  default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
  put a = gput (from a)
 
  get :: [Bit] -> (a, [Bit])
 
  default get :: (Generic a, GSerialize (Rep a)) => [Bit] -> (a, [Bit])
  get xs = (to x, xs')
    where (x, xs') = gget xs
 
class GSerialize f where
  gput :: f a -> [Bit]
  gget :: [Bit] -> (f a, [Bit])
 

-- | Unit: used for constructors without arguments
instance GSerialize U1 where
  gput U1 = []
  gget xs = (U1, xs)
 
-- | Constants, additional parameters and recursion of kind *
instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput (a :*: b) = gput a ++ gput b
  gget xs = (a :*: b, xs'')
    where (a, xs') = gget xs
          (b, xs'') = gget xs'
 
-- | Meta-information (constructor names, etc.)
instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput (L1 x) = O : gput x
  gput (R1 x) = I : gput x
  gget (O:xs) = (L1 x, xs')
    where (x, xs') = gget xs
  gget (I:xs) = (R1 x, xs')
    where (x, xs') = gget xs
 
-- | Sums: encode choice between constructors
instance (GSerialize a) => GSerialize (M1 i c a) where
  gput (M1 x) = gput x
  gget xs = (M1 x, xs')
    where (x, xs') = gget xs
 
-- | Products: encode multiple arguments to constructors
instance (Serialize a) => GSerialize (K1 i a) where
  gput (K1 x) = put x
  gget xs = (K1 x, xs')
    where (x, xs') = get xs
 
instance Serialize Bool where
  put True = [I]
  put False = [O]
  get (I:xs) = (True, xs)
  get (O:xs) = (False, xs)
 
--
-- Try it out. (Normally this would be in a separate module.)
--
 
data UserTree a = Node a (UserTree a) (UserTree a) | Leaf
  deriving (Generic, Show)
 
instance (Serialize a) => Serialize (UserTree a)
 
main = do
  let xs = put True
  print (fst . get $ xs :: Bool)
  let ys = put (Leaf :: UserTree Bool)
  print (fst . get $ ys :: UserTree Bool)
  let zs = put (Node False Leaf Leaf :: UserTree Bool)
  print (fst . get $ zs :: UserTree Bool)

-- naive implementation

-- | Unit: used for constructors without arguments
data U1 p = U1

-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p

type RealRepUserTree a =
  -- Information about the datatype
  M1 D Data_UserTree (
  -- Leaf, with information about the constructor
      M1 C Con_Leaf U1
  -- Node, with information about the constructor
  :+: M1 C Con_Node (
            -- Constructor argument, which could have information
            -- about a record selector label
            M1 S NoSelector (
              -- Argument, tagged with P because it is a parameter
              K1 P a)
        -- Another argument, tagged with R because it is 
        -- a recursive occurrence of a type
        :*: M1 S NoSelector (K1 R (UserTree a))
        -- Idem
        :*: M1 S NoSelector (K1 R (UserTree a))
  ))

class Generic a where
  -- Encode the representation of a user datatype
  type Rep a :: * -> *
  -- Convert from the datatype to its representation
  from  :: a -> (Rep a) x
  -- Convert from the representation to the datatype
  to    :: (Rep a) x -> a

instance Generic (UserTree a) where
  type Rep (UserTree a) = RepUserTree a
 
  from Leaf         = L1 U1
  from (Node a l r) = R1 (a :*: l :*: r)
 
  to (L1 U1)              = Leaf
  to (R1 (a :*: l :*: r)) = Node a l r