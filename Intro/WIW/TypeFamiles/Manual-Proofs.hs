-- Manual proofs
-- One of most deep results in computer science, the Curry–Howard correspondence, is the relation that logical propositions can be modeled by types and instantiating those types constitute proofs of these propositions. Programs are proofs and proofs are programs.

{-

Types	    Logic
A	        proposition
a : A	    proof
B(x)	    predicate
Void	    ⊥
Unit	    ⊤
A + B	    A ∨ B
A × B	    A ∧ B
A -> B	    A ⇒ B

In dependently typed languages we can exploit this result to its full extent, in Haskell we don't have the strength that dependent types provide but can still prove trivial results. For example, now we can model a type level function for addition and provide a small proof that zero is an additive identity.

P 0                   [ base step ]
∀n. P n  → P (1+n)    [ inductive step ]
-------------------
∀n. P(n)
Axiom 1: a + 0 = a
Axiom 2: a + suc b = suc (a + b)

  0 + suc a
= suc (0 + a)  [by Axiom 2]
= suc a        [Induction hypothesis]
∎

Translated into Haskell our axioms are simply type definitions and recursing over the inductive datatype constitutes the inductive step of our proof.


-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}

data Z
data S n

data SNat n where
  Zero :: SNat Z
  Succ :: SNat n -> SNat (S n)

data Eql a b where
  Refl :: Eql a a

type family Add m n
type instance Add Z n = n
type instance Add (S m) n = S (Add m n)

add :: SNat n -> SNat m -> SNat (Add n m)
add Zero     m = m
add (Succ n) m = Succ (add n m)

cong :: Eql a b -> Eql (f a) (f b)
cong Refl = Refl

-- ∀n. 0 + suc n = suc n
plus_suc :: forall n.  SNat n
         -> Eql (Add Z (S n)) (S n)
plus_suc Zero = Refl
plus_suc (Succ n) = cong (plus_suc n)

-- ∀n. 0 + n = n
plus_zero :: forall n. SNat n
         -> Eql (Add Z n) n
plus_zero Zero = Refl
plus_zero (Succ n) = cong (plus_zero n)

-- Using the TypeOperators extension we can also use infix notation at the type-level.

data a :=: b where
  Refl :: a :=: a

cong :: a :=: b -> (f a) :=: (f b)
cong Refl = Refl

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Zero     :+ m = m
type instance (Succ n) :+ m = Succ (n :+ m)

plus_suc :: forall n m. SNat n -> SNat m -> (n :+ (S m)) :=: (S (n :+ m))
plus_suc Zero m = Refl
plus_suc (Succ n) m = cong (plus_suc n m)

-- Advanced Proofs

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

import Data.Type.Equality

data Nat = Z | S Nat

data SNat n where
  Zero :: SNat Z
  Succ :: SNat n -> SNat (S n)

data Vec :: * -> Nat -> * where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

instance Show a => Show (Vec a n) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

type family (m :: Nat) :+ (n :: Nat) :: Nat where
  Z :+ n = n
  S m :+ n = S (m :+ n)

-- (a ~ b) implies (f a ~ f b)
cong :: a :~: b -> f a :~: f b
cong Refl = Refl

-- (a ~ b) implies (f a) implies (f b)
subst :: a :~: b -> f a -> f b
subst Refl = id

plus_zero :: forall n. SNat n -> (n :+ Z) :~: n
plus_zero Zero = Refl
plus_zero (Succ n) = cong (plus_zero n)

plus_suc :: forall n m. SNat n -> SNat m -> (n :+ (S m)) :~: (S (n :+ m))
plus_suc Zero m = Refl
plus_suc (Succ n) m = cong (plus_suc n m)

size :: Vec a n -> SNat n
size Nil         = Zero
size (Cons _ xs) = Succ $ size xs

reverse :: forall n a. Vec a n -> Vec a n
reverse xs = subst (plus_zero (size xs)) $ go Nil xs
  where
    go :: Vec a m -> Vec a k -> Vec a (k :+ m)
    go acc Nil = acc
    go acc (Cons x xs) = subst (plus_suc (size xs) (size acc)) $ go (Cons x acc) xs

append :: Vec a n -> Vec a m -> Vec a (n :+ m)
append (Cons x xs) ys = Cons x (append xs ys)
append Nil         ys = ys

vec :: Vec Int (S (S (S Z)))
vec = 1 `Cons` (2 `Cons` (3 `Cons` Nil))

test :: Vec Int (S (S (S Z)))
test = Main.reverse vec

-- One might consider whether we could avoid using the singleton trick and just use type-level natural numbers, and technically this approach should be feasible although it seems that the natural number solver in GHC 7.8 can decide some properties but not the ones needed to complete the natural number proofs for the reverse functions.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (Eq)
import GHC.TypeLits
import Data.Type.Equality

type Z = 0

type family S (n :: Nat) :: Nat where
  S n = n + 1

-- Yes!
eq_zero :: Z :~: Z
eq_zero = Refl

-- Yes!
zero_plus_one :: (Z + 1) :~: (1 + Z)
zero_plus_one = Refl

-- Yes!
plus_zero :: forall n. (n + Z) :~: n
plus_zero = Refl

-- Yes!
plus_one :: forall n. (n + S Z) :~: S n
plus_one = Refl

-- No.
plus_suc :: forall n m. (n + (S m)) :~: (S (n + m))
plus_suc = Refl

-- Caveat should be that there might be a way to do this in GHC 7.6 that I'm not aware of. In GHC 7.10 there are some planned changes to solver that should be able to resolve these issues. In particular there are plans to allow pluggable type system extensions that could outsource these kind of problems to third party SMT solvers which can solve these kind of numeric relations and return this information back to GHC's typechecker.

-- As an aside this is a direct transliteration of the equivalent proof in Agda, which is accomplished via the same method but without the song and dance to get around the lack of dependent types.

module Vector where

infixr 10 _∷_

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ    #-}
{-# BUILTIN ZERO    zero #-}
{-# BUILTIN SUC     suc  #-}

infixl 6 _+_

_+_ : ℕ → ℕ → ℕ
0 + n = n
suc m + n = suc (m + n)

data Vec (A : Set) : ℕ → Set where
  []  : Vec A 0
  _∷_ : ∀ {n} → A → Vec A n → Vec A (suc n)

_++_ : ∀ {A n m} → Vec A n → Vec A m → Vec A (n + m)
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infix 4 _≡_

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

subst : {A : Set} → (P : A → Set) → ∀{x y} → x ≡ y → P x → P y
subst P refl p = p

cong : {A B : Set} (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

vec : ∀ {A} (k : ℕ) → Set
vec {A} k = Vec A k

plus_zero : {n : ℕ} → n + 0 ≡ n 
plus_zero {zero}  = refl
plus_zero {suc n} = cong suc plus_zero

plus_suc : {n : ℕ} → n + (suc 0) ≡ suc n 
plus_suc {zero}  = refl
plus_suc {suc n} = cong suc (plus_suc {n})

reverse : ∀ {A n} → Vec A n → Vec A n
reverse []       = []
reverse {A} {suc n} (x ∷ xs) = subst vec (plus_suc {n}) (reverse xs ++ (x  ∷ []))