-- Promoted symbols

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}


import GHC.TypeLits
import Data.Type.Equality

data Label (l :: Symbol) = Get

class Has a l b | a l -> b where
  from :: a -> Label l -> b

data Point2D = Point2 Double Double deriving Show
data Point3D = Point3 Double Double Double deriving Show

instance Has Point2D "x" Double where
  from (Point2 x _) _ = x

instance Has Point2D "y" Double where
  from (Point2 _ y) _ = y


instance Has Point3D "x" Double where
  from (Point3 x _ _) _ = x

instance Has Point3D "y" Double where
  from (Point3 _ y _) _ = y

instance Has Point3D "z" Double where
  from (Point3 _ _ z) _ = z


infixl 6 #

(#) :: a -> (a -> b) -> b
(#) = flip ($)

_x :: Has a "x" b => a -> b
_x pnt = from pnt (Get :: Label "x")

_y :: Has a "y" b => a -> b
_y pnt = from pnt (Get :: Label "y")

_z :: Has a "z" b => a -> b
_z pnt = from pnt (Get :: Label "z")

type Point a r = (Has a "x" r, Has a "y" r)

distance :: (Point a r, Point b r, Floating r) => a -> b -> r
distance p1 p2 = sqrt (d1^2 + d2^2)
  where
    d1 = (p1 # _x) + (p1 # _y)
    d2 = (p2 # _x) + (p2 # _y)

main :: IO ()
main = do
  print $ (Point2 10 20) # _x

  -- Fails with: No instance for (Has Point2D "z" a0)
  -- print $ (Point2 10 20) # _z

  print $ (Point3 10 20 30) # _x
  print $ (Point3 10 20 30) # _z

  print $ distance (Point2 1 3) (Point2 2 7)
  print $ distance (Point2 1 3) (Point3 2 7 4)
  print $ distance (Point3 1 3 5) (Point3 2 7 3)

-- Since record is fundamentally no different from the tuple we can also do the same kind of construction over record field names.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}


import GHC.TypeLits

newtype Field (n :: Symbol) v = Field { unField :: v }
  deriving Show

data Person1 = Person1
  { _age      :: Field "age" Int
  , _name     :: Field "name" String
  }

data Person2 = Person2
  { _age'  :: Field "age" Int
  , _name' :: Field "name" String
  , _lib'  :: Field "lib" String
  }

deriving instance Show Person1
deriving instance Show Person2

data Label (l :: Symbol) = Get

class Has a l b | a l -> b where
  from :: a -> Label l -> b

instance Has Person1 "age" Int where
  from (Person1 a _) _ = unField a

instance Has Person1 "name" String where
  from (Person1 _ a) _ = unField a

instance Has Person2 "age" Int where
  from (Person2 a _ _) _ = unField a

instance Has Person2 "name" String where
  from (Person2 _ a _) _ = unField a

age :: Has a "age" b => a -> b
age pnt = from pnt (Get :: Label "age")

name :: Has a "name" b => a -> b
name pnt = from pnt (Get :: Label "name")

-- Parameterized constraint kind for "Simon-ness" of a record.
type Simon a = (Has a "name" String, Has a "age" Int)

spj :: Person1
spj = Person1 (Field 56) (Field "Simon Peyton Jones")

smarlow :: Person2
smarlow = Person2 (Field 38) (Field "Simon Marlow") (Field "rts")


catNames :: (Simon a, Simon b) => a -> b -> String
catNames a b = name a ++ name b

addAges :: (Simon a, Simon b) => a -> b -> Int
addAges a b = age a + age b


names :: String
names = name smarlow ++ "," ++ name spj
-- "Simon Marlow,Simon Peyton Jones"

ages :: Int
ages = age spj + age smarlow
-- 94

-- Notably this approach is mostly just all boilerplate class instantiation which could be abstracted away using TemplateHaskell or a Generic deriving.