-- TypeLevel Operations

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

-- a ≡ b
data Eql a b where
  Refl :: Eql a a

-- Congruence
-- (f : A → B) {x y} → x ≡ y → f x ≡ f y
cong :: Eql a b -> Eql (f a) (f b)
cong Refl = Refl

-- Symmetry
-- {a b : A} → a ≡ b → a ≡ b
sym :: Eql a b -> Eql b a
sym Refl = Refl

-- Transitivity
-- {a b c : A} → a ≡ b → b ≡ c → a ≡ c
trans :: Eql a b -> Eql b c -> Eql a c
trans Refl Refl = Refl

-- Coerce one type to another given a proof of their equality.
-- {a b : A} → a ≡ b → a → b
castWith :: Eql a b -> a -> b
castWith Refl = id

-- Trivial cases
a :: forall n. Eql n n
a = Refl

b :: forall. Eql () ()
b = Refl

-- Another DataKinds implementation
-- ref: https://stackoverflow.com/questions/22082852/erratic-hole-type-resolution

{-# LANGUAGE
    DataKinds, PolyKinds, TypeFamilies, 
    UndecidableInstances, GADTs, TypeOperators #-}

data (==) :: k -> k -> * where
    Refl :: x == x

sym :: a == b -> b == a
sym Refl = Refl 

data Nat = Zero | Succ Nat

data SNat :: Nat -> * where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

type family a + b where
    Zero   + b = b
    Succ a + b = Succ (a + b)

addAssoc :: SNat a -> SNat b -> SNat c -> (a + (b + c)) == ((a + b) + c)
addAssoc SZero b c = Refl
addAssoc (SSucc a) b c = case addAssoc a b c of Refl -> Refl

addComm :: SNat a -> SNat b -> (a + b) == (b + a)
addComm SZero SZero = Refl
addComm (SSucc a) SZero = case addComm a SZero of Refl -> Refl
addComm SZero (SSucc b) = case addComm SZero b of Refl -> Refl
addComm sa@(SSucc a) sb@(SSucc b) =
    case addComm a sb of
        Refl -> case addComm b sa of
            Refl -> case addComm a b of
                Refl -> Refl 