-- Concepts
-- ref: http://v.youku.com/v_show/id_XMTQ4ODgxNTMwMA==.html?from=y1.7-2

-- Kind (poly kind/entity kind/data kind) (type's type / type constructor)
-- Type (poly/type-class-restricted/entity(mono)) - typeclass 
-- Term (function/operator/value)


-- Term
    -- value
    -- '1'/True/100
    
    -- function
    -- id/const/flip/($)/map/not

-- Type
    -- basic type
    -- Int/Char/String
    
    -- polymorphic type
    -- [a]/Maybe a/Tree a/(a, b)

    -- Every value must have a specific type

    -- Define type
        -- enum type (ADT)
        -- data Bool = True | False

        -- parameterized type (GADT)
        -- data Maybe a = Nothing | Just a

        -- recursive type
        -- data Nat = Zero | Succ Nat

        -- constructor type
        -- data Pair a b = Pair a b -- same name
        -- data Person = Person { name :: String, age :: Int }

    -- type function

-- Typeclass (constructor class / type function)

    -- Type's properties
    -- overload

    {-

        现在，我们除了Type Constructor的概念，还有了Kind的概念，终于可以引入Class了。简而言之，一个Type Class抽象了“能够按照某种要求完成某种任务”的某个特定Kind的Type Constructor。比如：

        class Monoid :: * ，抽象了某些Kind为*的类型，只要这些类型能够提供一个零元和满足结合律的二元算子；
        class Functor :: * -> *，抽象了某些Kind为* -> *的constructor，只要这些constructor能够满足Functor laws，至于constructor的本质是什么，是数据的容器也好，还是能够生成数据的计算过程也好，无妨。
    
    -}

    {-
    
        实际上，Haskell的Kind System以及Type Class还有许多重要的扩展：

        1. Class/Instance Declaration处可以有Context，比如class Eq a => class Ord a，或者instance Show a => instance Show [a]。
        
        2. Kind Polymorphism，一个Class的Kind可以不固定。参考7.8.Kind polymorphism
        
        3. Data Kinds，可以把部分datatype给promote成kind，现在不仅可以有*，还可以有Type Level Nat/String等等，可以在Haskell里实行有限的dependently typed programming。参考7.9.Datatype promotion
        
        4. Type families和Multi-parameter type class，等等
   
    -}

-- Kind
    -- Type is category of Term
    -- Class gave Type properies
    -- Kind is category of Type

    -- Maybe Kind :: * -> *
    -- Maybe Int  :: *
    -- Either :: * -> * -> *
    -- (,) :: * -> * -> *

    -- Entity type
        -- Only when kind is * -> Type has value

    -- Polymorphic kind
        -- class Typeable a where   -- a :: *
            -- typeOf :: a -> String
        -- instance Typeable Int where -- Int :: *
            -- typeOf _ = "Int"        
        -- instance Typeable2 Maybe...
        -- instance Typeable3 Either...

        -- class Typeable (t :: k) where
            -- typeOf :: Proxy t -> String  -- t has no value, t's kind is polymorphic k instead of *. Directly use t will cause error
        -- data Proxy (a :: k) = Proxy
            -- kind of Proxy is k -> *
        -- instance Typeable Maybe where
            -- typeOf Proxy = "Maybe"

data Tree k a = Leaf a | Node a (k (Tree k a))

type RoseTree a = Tree [] a
type BinTree a = Tree Pair a

data Pair a = MKPair a

-- Type family
    -- class IsList l where
        -- type Item l   -- must specify what Item is for types
        -- fromList :: [Item l] -> l
        -- toList :: l -> [Item l]
    -- instance (Ord a) -> (IsList Set.Set a) where
        -- type (Item (Set.Set a)) = a
        -- fromList = Set.fromList
        -- toList = Set.toList

    -- DataKinds

{-# LANGUAGE DataKinds #-}        

data Nat = Z | S Nat deriving (Eq, Show)
-- Nat is kinds, Z, S is types
-- S :: Nat -> Nat
-- Z :: Nat

type family (a :: Nat) + (b :: Nat) :: Nat where
    Z + m = m
    S n + m = n + S m

type family (a :: Nat) * (b :: Nat) :: Nat where
    Z * m = Z
    S n * m = n * m + m

-- data Vec :: Nat -> * where
    -- Nil :: Vec Z
    -- Cons :: Int -> Vec n -> Vec (S n)        
data Vec a (n :: Nat) where
    Nil :: Vec a Z
    Cons :: a -> Vec a n -> Vec a (S n)
-- Vec is a kind :: * -> Nat -> *


deriving instance Show a => Show (Vec a n)

-- If vec is empty, then vec cannot match vhead/vtail
vhead :: Vec a (S n) -> a
vhead (Cons a v) = a

vtail :: Vec a (S n) -> Vec a n
vtail (Cons x xs) = xs

vappend :: Vec a n -> Vec a m -> Vec a (n + m)
vappend (Cons x xs) ys = Cons x (vappend xs ys)
vappend Nil ys = ys

-- OverloadedRecordField

data Node = Layer {
        ident :: String,
        props :: [(String, Float)],
        image :: String
    } | Label {
        ident :: String,
        props :: [(String, Float)],
        image :: String
    }

-- Algebraic data type
-- :+:/:*:

data (:*:) a b = a :*: b
data (:+:) a b = L a | R b

class Generic a where
    type Rep a :: *
    from :: a -> Rep a
    to :: Rep a -> a

instance Generic Bool where
    type Rep Bool = U :+: U
    from = fromBool
    to = toBool

class GEq a where
    geq :: a -> a -> Bool
    default geq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
    geq a b = geq (from a) (from b)

instance (GEq a, GEq b) => GEq (a :+: b) where
    geq (L a1) (L a2) = geq a1 a2
    geq (R b1) (R b2) = geq b1 b2
    geq _ _ = False

instance (GEq a, GEq b) => GEq (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 .&. geq b1 b2

instance GEq U where
    geq U U = True

-- DeriveAnyClass

-- idA - A --to/from-- Rep A - idB
--       |               | 
--instance --to/from-- instance

-- If A - B 同构 -> Rep A -> Rep B

data UI p = UI deriving (Eq, Show)
data (:*:) a b p = a p :*: b p deriving (Eq, Show)
data (:+:) a b p = L (a p) | R (b p) deriving (Eq, Show)

class Generic (a :: *) where
    type family Rep a :: * -> *
    from :: a -> Rep a x
    to :: Rep a x -> a

class GenericI (f :: * -> *) where
    type RepI f :: * -> *
    fromI :: f p -> RepI f p
    toI :: RepI f p -> f p

newtype Par p = Par { unPar :: p } deriving Show
newtype Rec a p = Rec { unRec :: a p } deriving Show

data List a = Nil | Cons a (List a)
data Tree a = Leaf | Node a (Tree a) (Tree a)

instance GenericI List where
    type RepI List = UI :+: (Par :*: (Rec List))
    fromI Nil = L UI
    fromI (Cons a xs) = R (Par a :*: Rec xs)
    toI (L UI) = Nil
    toI (R (Par a :*: Rec xs)) = Cons a xs

instance GenericI Tree where
    type RepI Tree = UI :+: (Par :*: (Rec Tree) :*: (Rec Tree))
    fromI Leaf = L UI
    fromI (Node n l r) = R (Par n :*: Rec l :*: Rec r)
    toI (L UI) = Leaf
    toI (R (Par n :*: Rec l :*: Rec r)) = Node n l r

class GFunctor (f :: * -> *) where
    gfmap :: (a -> b) -> (f a -> f b)
    default gfmap :: (GenericI f, GFunctor (RepI f)) => (a -> b) -> (f a -> f b)
    gfmap f x = toI (gfmap f (fromI x))

instance GFunctor UI where
    gfmap f UI = UI

instance (GFunctor a, GFunctor b) => GFunctor (a :*: b) where
    gfmap f (a :*: b) = gfmap f a :*: gfmap f b

instance (GFunctor a, GFunctor b) => GFunctor (a :+: b) where
    gfmap f (L a) = L (gfmap f a)
    gfmap f (R a) = R (gfmap g a)

instance GFunctor Par where
    gfmap f (Par a) = Par (f p)

instance GFunctor Tree
instance GFunctor List

newtype MI i e (f :: * -> *) p = MI { unMI :: f p }
data D -- data type
data C -- data constructor type
data S -- record type
type DI = MI D
type CI = MI C
type SI = MI S
class Datatype d
class Selector s
class Constructor c