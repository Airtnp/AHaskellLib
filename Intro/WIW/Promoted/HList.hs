-- HList

-- A heterogeneous list is a cons list whose type statically encodes the ordered types of its values.

-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

infixr 5 :::

data HList (ts :: [ * ]) where
  Nil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

-- Take the head of a non-empty list with the first value as Bool type.
headBool :: HList (Bool ': xs) -> Bool
headBool hlist = case hlist of
  (a ::: _) -> a

hlength :: HList x -> Int
hlength Nil = 0
hlength (_ ::: b) = 1 + (hlength b)


tuple :: (Bool, (String, (Double, ())))
tuple = (True, ("foo", (3.14, ())))

hlist :: HList '[Bool, String , Double , ()]
hlist = True ::: "foo" ::: 3.14 ::: () ::: Nil

-- Of course this immediately begs the question of how to print such a list out to a string in the presence of type-heterogeneity. In this case we can use type-families combined with constraint kinds to apply the Show over the HLists parameters to generate the aggregate constraint that all types in the HList are Showable, and then derive the Show instance.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.Exts (Constraint)

infixr 5 :::

data HList (ts :: [ * ]) where
  Nil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

type family Map (f :: a -> b) (xs :: [a]) :: [b]
type instance Map f '[] = '[]
type instance Map f (x ': xs) = f x ': Map f xs

type family Constraints (cs :: [Constraint]) :: Constraint
type instance Constraints '[] = ()
type instance Constraints (c ': cs) = (c, Constraints cs)

type AllHave (c :: k -> Constraint) (xs :: [k]) = Constraints (Map c xs)

showHList :: AllHave Show xs => HList xs -> [String]
showHList Nil = []
showHList (x ::: xs) = (show x) : showHList xs

instance AllHave Show xs => Show (HList xs) where
  show = show . showHList

example1 :: HList '[Bool, String , Double , ()]
example1 = True ::: "foo" ::: 3.14 ::: () ::: Nil
-- ["True","\"foo\"","3.14","()"]