-- Data
-- Just as Typeable lets us create runtime type information, the Data class allows us to reflect information about the structure of datatypes to runtime as needed.

class Typeable a => Data a where
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
          -> (forall g. g -> c g)
          -> a
          -> c a

  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  toConstr :: a -> Constr
  dataTypeOf :: a -> DataType
  gmapQl :: (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r

-- The types for gfoldl and gunfold are a little intimidating ( and depend on RankNTypes ), the best way to understand is to look at some examples. First the most trivial case a simple sum type Animal would produce the following code:

data Animal = Cat | Dog deriving Typeable

instance Data Animal where
  gfoldl k z Cat = z Cat
  gfoldl k z Dog = z Dog

  gunfold k z c
    = case constrIndex c of
        1 -> z Cat
        2 -> z Dog

  toConstr Cat = cCat
  toConstr Dog = cDog

  dataTypeOf _ = tAnimal

tAnimal :: DataType
tAnimal = mkDataType "Main.Animal" [cCat, cDog]

cCat :: Constr
cCat = mkConstr tAnimal "Cat" [] Prefix

cDog :: Constr
cDog = mkConstr tAnimal "Dog" [] Prefix

instance Data a => Data [a] where
  gfoldl _ z []     = z []
  gfoldl k z (x:xs) = z (:) `k` x `k` xs

  toConstr []    = nilConstr
  toConstr (_:_) = consConstr

  gunfold k z c
    = case constrIndex c of
        1 -> z []
        2 -> k (k (z (:)))

  dataTypeOf _ = listDataType

nilConstr :: Constr
nilConstr = mkConstr listDataType "[]" [] Prefix

consConstr :: Constr
consConstr = mkConstr listDataType "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

-- Looking at gfoldl we see the Data has an implementation of a function for us to walk an applicative over the elements of the constructor by applying a function k over each element and applying z at the spine. For example look at the instance for a 2-tuple as well:

instance (Data a, Data b) => Data (a,b) where
  gfoldl k z (a,b) = z (,) `k` a `k` b

  toConstr (_,_) = tuple2Constr

  gunfold k z c
    = case constrIndex c of
      1 -> k (k (z (,)))

  dataTypeOf _  = tuple2DataType

tuple2Constr :: Constr
tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix

tuple2DataType :: DataType
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]

-- This is pretty neat, now within the same typeclass we have a generic way to introspect any Data instance and write logic that depends on the structure and types of its subterms. We can now write a function which allows us to traverse an arbitrary instance of Data and twiddle values based on pattern matching on the runtime types. So let's write down a function over which increments a Value type for both for n-tuples and lists.

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Control.Monad.Identity
import Control.Applicative

data Animal = Cat | Dog deriving (Data, Typeable)

newtype Val = Val Int deriving (Show, Data, Typeable)

incr :: Typeable a => a -> a
incr = maybe id id (cast f)
  where f (Val x) = Val (x * 100)

over :: Data a => a -> a
over x = runIdentity $ gfoldl cont base (incr x)
  where
    cont k d = k <*> (pure $ over d)
    base = pure


example1 :: Constr
example1 = toConstr Dog
-- Dog

example2 :: DataType
example2 = dataTypeOf Cat
-- DataType {tycon = "Main.Animal", datarep = AlgRep [Cat,Dog]}

example3 :: [Val]
example3 = over [Val 1, Val 2, Val 3]
-- [Val 100,Val 200,Val 300]

example4 :: (Val, Val, Val)
example4 = over (Val 1, Val 2, Val 3)
-- (Val 100,Val 200,Val 300)

-- We can also write generic operations, for example to count the number of parameters in a data type.

numHoles :: Data a => a -> Int
numHoles = gmapQl (+) 0 (const 1)

example1 :: Int
example1 = numHoles (1,2,3,4,5,6,7)
-- 7

example2 :: Int
example2 = numHoles (Just 3)
-- 1