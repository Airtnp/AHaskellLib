-- F-Algebra
-- ref: https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
-- The initial algebra approach differs from the final interpreter approach in that we now represent our terms as algebraic datatypes and the interpreter implements recursion and evaluation occurs through pattern matching.


type Algebra f a = f a -> a
type Coalgebra f a = a -> f a
newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
ana  :: Functor f => Coalgebra f a -> a -> Fix f
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b

-- In Haskell a F-algebra is a functor f a together with a function f a -> a. A coalgebra reverses the function. For a functor f we can form its recursive unrolling using the recursive Fix newtype wrapper.

newtype Fix f = Fix { unFix :: f (Fix f) }

Fix :: f (Fix f) -> Fix f
unFix :: Fix f -> f (Fix f)
-- Fix f = f (f (f (f (f (f ( ... ))))))

newtype T b a = T (a -> b)

{-
    Fix (T a)
    Fix T -> a
    (Fix T -> a) -> a
    (Fix T -> a) -> a -> a
    ...
-}

-- In this form we can write down a generalized fold/unfold function that are datatype generic and written purely in terms of the recursing under the functor.

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- We call these functions catamorphisms and anamorphisms. Notice especially that the types of these two functions simply reverse the direction of arrows. Interpreted in another way they transform an algebra/coalgebra which defines a flat structure-preserving mapping between Fix f f into a function which either rolls or unrolls the fixpoint. What is particularly nice about this approach is that the recursion is abstracted away inside the functor definition and we are free to just implement the flat transformation logic!

    -- catamorphism

    -- This way of looking at things provides a simple route to designing fold-like functions on other algebraic data structures, like various sorts of trees. One writes a function which recursively replaces the constructors of the datatype with provided functions, and any constant values of the type with provided values. Such functions are generally referred to as Catamorphisms.


-- Natural number in F-algebra
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

newtype Fix f = Fix { unFix :: f (Fix f) }

-- catamorphism
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- anamorphism
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- hylomorphism
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo f g = cata f . ana g

type Nat = Fix NatF
data NatF a = S a | Z deriving (Eq,Show)

instance Functor NatF where
  fmap f Z     = Z
  fmap f (S x) = S (f x)

plus :: Nat -> Nat -> Nat
plus n = cata phi where
  phi Z     = n
  phi (S m) = s m

times :: Nat -> Nat -> Nat
times n = cata phi where
  phi Z     = z
  phi (S m) = plus n m

int :: Nat -> Int
int = cata phi where
  phi  Z    = 0
  phi (S f) = 1 + f

nat :: Integer -> Nat
nat = ana (psi Z S) where
  psi f _ 0 = f
  psi _ f n = f (n-1)

z :: Nat
z = Fix Z

s :: Nat -> Nat
s = Fix . S


type Str = Fix StrF
data StrF x = Cons Char x | Nil

instance Functor StrF where
  fmap f (Cons a as) = Cons a (f as)
  fmap f Nil = Nil

nil :: Str
nil = Fix Nil

cons :: Char -> Str -> Str
cons x xs = Fix (Cons x xs)

str :: Str -> String
str = cata phi where
  phi Nil         = []
  phi (Cons x xs) = x : xs

str' :: String -> Str
str' = ana (psi Nil Cons) where
  psi f _ []     = f
  psi _ f (a:as) = f a as

map' :: (Char -> Char) -> Str -> Str
map' f = hylo g unFix
  where
    g Nil        = Fix Nil
    g (Cons a x) = Fix $ Cons (f a) x


type Tree a = Fix (TreeF a)
data TreeF a f = Leaf a | Tree a f f deriving (Show)

instance Functor (TreeF a) where
  fmap f (Leaf a) = Leaf a
  fmap f (Tree a b c) = Tree a (f b) (f c)

depth :: Tree a -> Int
depth = cata phi where
  phi (Leaf _)     = 0
  phi (Tree _ l r) = 1 + max l r


example1 :: Int
example1 = int (plus (nat 125) (nat 25))
-- 150

-- Or for example an interpreter for a small expression language that depends on a scoping dictionary.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import qualified Data.Map as M

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo f g = cata f . ana g

type Id = String
type Env = M.Map Id Int

type Expr = Fix ExprF
data ExprF a
  = Lit Int
  | Var Id
  | Add a a
  | Mul a a
  deriving (Show, Eq, Ord, Functor)

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)

eval :: M.Map Id Int -> Fix ExprF -> Maybe Int
eval env = cata phi where
  phi ex = case ex of
    Lit c   -> pure c
    Var i   -> M.lookup i env
    Add x y -> liftA2 (+) x y
    Mul x y -> liftA2 (*) x y

expr :: Expr
expr = Fix (Mul n (Fix (Add x y)))
  where
    n = Fix (Lit 10)
    x = Fix (Var "x")
    y = Fix (Var "y")

env :: M.Map Id Int
env = M.fromList [("x", 1), ("y", 2)]

compose :: (f (Fix f) -> c) -> (a -> Fix f) -> a -> c
compose x y = x . unFix . y

example :: Maybe Int
example = eval env expr
-- Just 30

-- What's especially nice about this approach is how naturally catamorphisms compose into efficient composite transformations.

-- compose :: Functor f => (f (Fix f) -> c) -> (a -> Fix f) -> a -> c
-- compose f g = f . unFix . g