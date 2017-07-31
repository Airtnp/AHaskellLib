{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Nat = Z | S Nat deriving (Eq, Show)

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four

data Vec :: Nat -> * -> * where
  Nil :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

instance Show a => Show (Vec n a) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

class FromList n where
  fromList :: [a] -> Vec n a

instance FromList Z where
  fromList [] = Nil

instance FromList n => FromList (S n) where
  fromList (x:xs) = Cons x $ fromList xs


lengthVec :: Vec n a -> Nat
lengthVec Nil = Z
lengthVec (Cons x xs) = S (lengthVec xs)

zipVec :: Vec n a -> Vec n b -> Vec n (a,b)
zipVec Nil Nil = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x,y) (zipVec xs ys)

vec4 :: Vec Four Int
vec4 = fromList [0, 1, 2, 3]

vec5 :: Vec Five Int
vec5 = fromList [0, 1, 2, 3, 4]


example1 :: Nat
example1 = lengthVec vec4
-- S (S (S (S Z)))

example2 :: Vec Four (Int, Int)
example2 = zipVec vec4 vec4
-- Cons (0,0) (Cons (1,1) (Cons (2,2) (Cons (3,3) (Nil))))

example2 = zipVec vec4 vec5
-- Couldn't match type 'S 'Z with 'Z
-- Expected type: Vec Four Int
--   Actual type: Vec Five Int

-- The same technique we can use to create a container which is statically indexed by an empty or non-empty flag, such that if we try to take the head of an empty list we'll get a compile-time error, or stated equivalently we have an obligation to prove to the compiler that the argument we hand to the head function is non-empty.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Size = Empty | NonEmpty

data List a b where
  Nil  :: List Empty a
  Cons :: a -> List b a -> List NonEmpty a

head' :: List NonEmpty a -> a
head' (Cons x _) = x

example1 :: Int
example1 = head' (1 `Cons` (2 `Cons` Nil))

-- Cannot match type Empty with NonEmpty
example2 :: Int
example2 = head' Nil
Couldn't match type None with Many
Expected type: List NonEmpty Int
  Actual type: List Empty Int