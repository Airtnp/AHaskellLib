-- TODO: Define Hask category
-- ref : https://zhuanlan.zhihu.com/p/25407184
--     : https://zhuanlan.zhihu.com/p/25565309
--     : http://hongjiang.info/understand-monad-5-what-is-endofunctor/
--     : https://www.zhihu.com/question/48164238/answer/109748643
--     : https://hackage.haskell.org/package/category-extras
--     : https://en.wikibooks.org/wiki/Haskell/Category_theory
--     : http://science.raphael.poss.name/categories-from-scratch.html
--     : https://www.zhihu.com/question/60402335/answer/175679814
--     : https://www.zhihu.com/question/65645659

module Hask where

class Category cat where
    id :: cat a a -- identity hom
    (.) :: cat b c -> cat a b -> cat a c -- o hom

type Hask = (->)

-- http://hackage.haskell.org/package/base-4.9.1.0/docs/src/Control.Category.html#Category
instance Category Hask where
    id x = x
    (.) f g x = f (g x)

instance Category (->) where
    id = GHC.Base.id -- Prelude.id
    (.) = (GHC.Base..) -- (Prelude..)

-- Arrow
-- | Right-to-left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f

-- id . f = f
-- f . id = f
-- f . (g . h) = (f . g) . h

-- Then, Hask is a well-defined category.

-- Init object (No Value)
data Void

-- Final object (Value is itself)
-- or () in Haskell
data Unit = Unit

-- Then, any value a of type A can be represented as
-- f :: () -> A === a

curry :: ((a, b) -> c) -> a -> (b -> c)
curry f a b = f (a, b)

uncurry :: (a -> (b -> c)) -> (a, b) -> c
uncurry f (a, b) = f a b

-- curry . uncurry = id
-- uncurry . curry = id

-- Dual Category
-- C^op

type Op a b = b -> a
instance Category Op where
    id x = x
    (.) f g x = g (f x)

type Op_Unit = Void
type Op_Void = Unit

-- Void and Unit[()] is dual

-- Functor
-- Functor is hom between Categories
-- F id_C = id_D
-- F (f ._C g) = (F f) ._D (F g)

-- Type Constructor of Functors
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

-- A Functor-Constructor must apply to a Functor f
-- eg: (Functor Maybe), then Maybe is a Functor in Category

-- Endofunctor:: C -> C Functor
-- Every Functor in Haskell is Endofunctor in Hask Category

-- D -> C
class ContraFunctor f where
    contrafmap :: (b -> a) -> f a -> f b

-- contrafmap id = id
-- contrafmap (g . f) = fmap f . fmap g

-- combine functors
newtype Compose f g a = Compose {getCompose :: f (g a)}
    deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap k (Compose fga) = Compose $ fmap (fmap k) fga

type f :.: g = Compose f g

-- State = ((-> s)) :.: (, s)
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f st = State $ \s ->
        let (a, s') = runState st s
        in (f a, s')

-- Identity Functor
newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

idphi :: (Identity a) -> a
idphi = runIdentity

idpsi :: a -> (Identity a)
idpsi = Identity

fidphi :: Functor f => (Compose f Identity) a -> f a
fidphi (Compose x) = fmap runIdentity x

fidpsi :: Functor f => f a -> (Compose f Identity) a
fidpsi fa = Compose (fmap Identity fa)

-- Then, we have
-- Category of Categories => Cat Category
-- ob(C) = Category
-- hom(C) = Functor
-- Id(C) = Identity
-- o(C) = Compose

-- Hom Functor
-- Hom(C) is a hom-set (Hom_C(a, b))
-- This is a set category
-- C -> Hom_C(a, b) Functor => Hom Functor

-- Covariant Hom Functor
-- Fix a, b is varying
-- Then we have Hom_C(a, -) => Covariant Hom Functor (C -> Set[Hom_C(a, -)])
instance Functor ((->) a) where
    fmap f g = f . g

-- Contravariant Hom Functor
-- Fix b, a is varying
-- Then we have Hom_C(-, b) => Contravariant Hom Functor (C^op -> Set[Hom_C(-, b)])
instance ContraFunctor (Op a) where
    contrafmap f (Op g) = Op (g . f)

-- No fixing
-- Hom_C(-, -)
-- C^op x C -> Set[Hom_C(-, -)] => Hom Functor


-- Nature Transform
{-
    若我们有函子 F 和函子 G， 
    都是范畴C到范畴D之间的函子。

    那么对于范畴C中的任意对象 a，
    则在范畴D中分别有对应的对象F a 和G a，
    F a和G a之间存在函数t_a: F a -> G a。

    对于范畴C中的任意对象 b，
    则在范畴D中分别有对应的对象F b和G b，
    F b和G b之间存在函数t_b: F b -> G b。

    如果函数t_a和t_b满足一致性关系，
    即对于范畴C上的态射f: a -> b，
    满足交换关系G f o t_a = t_b o F f，
    则称变换t: F => G是函子 F和 G的自然变换。

    C:
    a    ----f---- b

    D:
    F a  ---t_a--- G a
     |              |
    F f            G f
     |              |
    F b  ---t_b--- G b
-}

-- No G f o t_a = t_b o F f restrict (manually ensure it in definition)
newtype Nat f g = Nat {runNat :: forall a. f a -> g a}

-- phi :: Nat f g
-- phi . fmap k = fmap k . phi

-- safeHead : Nat [] Maybe
-- maybeToList : Nat Maybe []
-- reverse : Nat [] []

-- Combine of Natural Transform

{-
    若函子F、函子G 和函子H 都是范畴C到范畴D的函子，
    \alpha: F => G 是函子F 到函子G 的自然变换，
    \beta: G => H是函子G 到函子H 的自然变换。
    
    则我们可以将\alpha和\beta组合起来，
    得到一个新的自然变换\beta o \alpha: F => H，
    这个组合又称为自然变换的垂直组合。
-}

vertComp :: (Functor f, Functor g, Functor (h :: k -> *))
            => Nat g h -> Nat f g -> Nat f h
vertComp beta@(Nat gh) alpha@(Nat fg) = Nat $ gh . fg

-- associtivity
{-
    gamma  `vertComp` (beta   `vertComp` alpha)
    = Nat hk `vertComp` (Nat gh `vertComp` Nat fg)
    = Nat ( hk . (gh  . fg))
    = Nat ((hk .  gh) . fg )
    = (Nat hk `vertComp` Nat gh) `vertComp` Nat fg
    = (gamma  `vertComp` beta)   `vertComp` alpha
-}

{-
    若函子F、函子F' 是范畴C到范畴D的函子，
    函子G、函子G‘是范畴D到范畴E的函子，
    \alpha: F => F'是函子F 到函子F' 的自然变换，
    \beta: G => G'是函子G 到函子G' 的自然变换。
    
    我们也可以将\alpha和\beta组合起来，
    得到一个新的自然变换\beta ◇ \alpha: G o F => G' o F'，
    这个组合又称为自然变换的水平组合，
    这里我们用和垂直组合不同的符号◇来表示水平组合。
-}

horzComp :: (Functor f, Functor f', Functor g, Functor g')
            => Nat g g' -> Nat f f' -> Nat (g :.: f) (g' :.: f')
            -- gg' :: g a -> g' a
            -- ff' :: f a -> f' a
            -- gfa :: g (f a)
            -- f'g' x :: f' (g' a)
            -- fmap ff' :: g' (f a) -> g' (f' a)
horzComp beta@(Nat gg') alpha@(Nat ff')
    = Nat $ \(Compose gfa) -> Compose (fmap ff' (gg' gfa))

-- associtivity
{-
    gamma  `vertComp` (beta   `vertComp` alpha)
    = Nat hk `vertComp` (Nat gh `vertComp` Nat fg)
    = Nat (Compose .  fmap (fmap fg  .  gh)      . hk  . getCompose)
    = Nat (Compose . (fmap (fmap fg) .  fmap gh) . hk  . getCompose)
    = Nat (Compose .  fmap (fmap fg) . (fmap gh  . hk) . getCompose)
    = (Nat hk `vertComp` Nat gh) `vertComp` Nat fg
    = (gamma  `vertComp` beta)   `vertComp` alpha
-}

-- 垂直组合和水平组合是可以互换的，
-- 先垂直复合再水平复合与先水平复合再垂直复合所得到的结果是相等的，
-- 即有(\beta' o \alpha') ◇ (\beta o \alpha) = (\beta' ◇ \beta) o (\alpha' ◇ \alpha)，
-- 自然变换的垂直水平互换定律。

{-
    特殊的自然变换的水平组合，
    一个函子F和自然变换\alpha: G => H的水平组合，
    可以将其看成是函子F 上的恒等自然同构id_F: F => F
    和自然变换\alpha: G => H的水平组合。
    
    当自然变换在前面时，
    称这个组合是前置水平组合，
    其形式为\alpha o F，定义为(\alpha o F) a = \alpha (F a)，
    有时为了书写更简明，前置水平组合简记为\alpha F

    当自然变换在后面时，
    称这个组合是后置组合，
    其形式为F o \alpha，定义为(F o \alpha) a = F (\alpha a)，
    有时为了书写更简明，后置水平组合简记为F \alpha。
-}

preFComp :: (Functor f, Functor g, Functor h)
            => Nat g h -> g (f a) -> h (f a)
preFComp (Nat gh) fa = gh fa

postFComp :: (Functor f, Functor g, Functor h)
            => Nat g h -> f (g a) -> f (h a)
postFComp (Nat gh) fa = fmap gh fa


-- Category of Functors
-- ob(F) : Functor(C -> D)
-- hom(F) : Natural Transform of ob(F)
-- o : vertComp
-- id : homomorphism

instance Category Nat where
    id = Nat id
    Nat f . Nat g = Nat (f . g)

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

-- Applicative/Monad

-- Since every Functor in Hask Category is Endofunctor (Hask -> Hask)
-- ob(Hask) = Functor
-- hom(Hask) = Natural Transform
-- Id(Hask)
-- o[⊗](Hask) tensor product

-- When ⊗ is Day convolution, we have Monoid (F, e, m)
-- id = e : id -> F
-- m = F ⊗ F -> F
-- F = Applicative

data Day f g a = forall b, c. Day (f b) (g c) ((b, c) -> a)

-- or
data Day f g a where
    Day :: forall u v. f u -> g v -> (u -> v -> a) -> Day f g a

liftA2 :: (u -> v -> a) -> F u -> F v -> F a
liftA2 :: Day F F a -> F a

instance Functor (Day f g) where
    fmap f (Day fb gc h) = Day (fb gc (f . h))

{-
    forall b, c. Day (f b) (g c) ((b, c) -> a)
    -- in Hask, ((a, b), c) ≅ (a, (b, c)), (a, b) ≅ (b, a)
    ＝ forall b, c. Day' (f b) ((b, c) -> a) (g c)
    ＝ forall c. Day2 (forall b. Day1 (f b) ((b, c) -> a)) (g c)
    -- curried (b, c) -> a ≅ b -> (c -> a) 
    ＝ forall c. Day2 (forall b. Day1 (f b) (b -> (c -> a)) (g c)
    -- coyoneda lemma, forall b. Coyoneda (b -> a) (f b) = f a 
    ＝ forall c. Day2 (f (c -> a)) (g c)

    day :: f (c -> a) -> g c -> Day f g a
    day fca gc = Day fca gc (uncurry id)

    dap :: Day f f a -> f a
    dap (Day2 fca fc) = fca <*> fc
-}

-- Applicative和Monad都是自函子范畴上的一个幺半群

-- Applicative是自函子的水平方向的组合
-- Applicative是自函子的Monoidal范畴上的一个幺半群，
-- 该Monoidal范畴的张量积（tensor product，⊗:M×M→M）是自函子的Day Convolution，
-- 单位元是Id functor。
class Functor f => Applicative f where
    pure :: a -> f a  -- e: Id -> F
    <*> :: f (a -> b) -> f a -> f b -- m: F `Day` F -> F

-- Monad是自函子的垂直方向的组合
-- Monad是自函子的Monoidal范畴上的一个幺半群，
-- 该Monoidal范畴的张量积（tensor product，⊗:M×M→M）是自函子的Compose，
-- 单位元是Id functor。
class Applicative m => Monad m where
  return :: a -> m a  -- e: Nat Id f
  (>>=) :: forall a b. m a -> (a -> m b) -> m b -- m: Nat (Compose f f) f
  -- Kleisli way
  (>=>)   :: forall a b c. (a -> m b) -> (b -> m c) -> (a -> m c)
  -- Join way
  join :: forall a. f (f a) -> f a
  join :: Compose F F a -> F a


{-
  join m = m >>= id
  join = id >=> id

  m >>= f = join (fmap f m)
  (>>=) = flip (id >=>)

  f >=> g = \x -> f x >>= g
  f >=> g = join . fmap g . f
-}

-- Arrow
-- transformation(arrow) vs. operation(Monad)

{-
    arr id = id
    arr (f >>> g) = arr f >>> arr g
    first (arr f) = arr (first f)
    first (f >>> g) = first f >>> first g
    first f >>> arr fst = arr fst >>> f
    first f >>> arr (id *** g) = arr (id *** g) >>> first f
    first (first f) >>> arr assoc = arr assoc >>> first f
-}

-- http://hackage.haskell.org/package/base-4.9.1.0/docs/src/Control.Arrow.html#Arrow
class Category a => Arrow a where
    arr :: (b -> c) -> a b c
    first :: a b c -> a (b, d) (c, d)
    first = (*** id)
    second :: a b c -> a (d, b) (d, c)
    second = (id ***)
    (***) :: a b c -> a b' c' -> a (b, b') (c, c')
    f *** g = first f >>> arr swap >>> first g >>> arr swap
      where swap ~(x,y) = (y,x)

-- ordinary function: (->) b c

newtype Reader r a = Reader { runReader :: r -> a }

instance Arrow Reader where
    arr f = f
    (***) f g ~(x, y) = (f x, g y)

instance Arrow (->) where
    arr f = f
    (***) f g ~(x, y) = (f x, g y)

-- Kleisli arrows: Monad m => (->) b (m c)

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    first (Kleisli f) = Kleisli $ \ ~(b,d) -> f b >>= \c -> return (c,d)
    second (Kleisli f) = Kleisli $ \ ~(d,b) -> f b >>= \c -> return (d,c)
    


-- stream transformer: (->) (Stream b) (Stream c)

-- simple automata
-- Another model of dataflow languages uses the type
        -- newtype Auto b c = Auto (b -> (c, Auto b c))

-- Fold - Catamorphism
-- ref: https://www.zhihu.com/question/37817937

-- Yoneda Embedding
-- ref : https://zhihu.com/question/23324349/answer/54242934
-- ref : https://bartoszmilewski.com/2013/05/15/understanding-yoneda/

-- Lens - van Laarhoven representation
-- ref : https://bartoszmilewski.com/2015/07/13/from-lenses-to-yoneda-embedding/

-- Profunctor

-- Kleisli Category


-- AST and Category

-- In category theory, a branch of mathematics, an initial object of a category C is an object I in C such that for every object X in C, there exists precisely one morphism I → X.
-- The dual notion is that of a terminal object (also called terminal element): T is terminal if for every object X in C there exists a single morphism X → T. Initial objects are also called coterminal or universal, and terminal objects are also called final.
-- If an object is both initial and terminal, it is called a zero object or null object. A pointed category is one with a zero object.

-- 在数学领域，范畴C的对象I称为始对象（或初始对象），若对任何对象X，从I到X的态射唯一，或者说，C(I,X)为单元素集合。终对象（或终止对象、终结对象）是始对象的对偶概念。范畴C的对象T称为终对象，若对任何对象X，从X到T的态射唯一。若某对象即是始对象又是终对象，则称其为零对象。

-- 范畴Set(以集合为对象，函数为态射)的唯一始对象为空集。空集到任何集合的态射只有唯一的一个空映射。任意单元素集合均为Set的终对象。任何集合到单元素集合只有一个把所有元素都映射到该单元素的态射。单元素集合之间互相同构。Set不存在零对象。

-- Hask Category -> AST Category (?)

    -- Initial Encoidng
        -- AST as datatype
        -- Functions operate on AST using pattern matching
        

    -- Final Encoding
        -- AST as type class
        -- Functions are instances of the type class
        -- Instance type denotes the semantic domain of AST
        -- 指称语义

-- Kan extension
data Lan k f a = forall b. Lan (k b -> a) (f b)

newtype Ran k f a = Ran { runRan :: forall b. (a -> k b) -> f b }