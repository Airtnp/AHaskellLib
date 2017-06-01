-- Type arithmetic
-- ref: https://wiki.haskell.org/Type_arithmetic
-- oleg: http://article.gmane.org/gmane.comp.lang.haskell.general/13223
-- 

-- Type arithmetic (or type-level computation) are calculations on the type-level, often implemented in Haskell using functional dependencies to represent functions.

-- type-level quick-sort

module Sort where
 
-- natural numbers
data Zero
data Succ a

-- booleans
data True
data False

-- lists
data Nil
data Cons a b

-- shortcuts
type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

-- example list
list1 :: Cons Three (Cons Two (Cons Four (Cons One Nil)))
list1 = undefined

-- utilities
numPred :: Succ a -> a
numPred = const undefined

class Number a where
numValue :: a -> Int

instance Number Zero where
numValue = const 0
instance Number x => Number (Succ x) where
numValue x = numValue (numPred x) + 1

numlHead :: Cons a b -> a
numlHead = const undefined

numlTail :: Cons a b -> b
numlTail = const undefined

class NumList l where
listValue :: l -> [Int]

instance NumList Nil where
listValue = const []
instance (Number x, NumList xs) => NumList (Cons x xs) where
listValue l = numValue (numlHead l) : listValue (numlTail l)

-- comparisons
data Less
data Equal
data Greater

class Cmp x y c | x y -> c

instance Cmp Zero Zero     Equal
instance Cmp Zero (Succ x) Less
instance Cmp (Succ x) Zero Greater
instance Cmp x y c => Cmp (Succ x) (Succ y) c

-- put a value into one of three lists according to a pivot element
class Pick c x ls eqs gs ls' eqs' gs' | c x ls eqs gs -> ls' eqs' gs'
instance Pick Less    x ls eqs gs (Cons x ls) eqs gs
instance Pick Equal   x ls eqs gs ls (Cons x eqs) gs
instance Pick Greater x ls eqs gs ls eqs (Cons x gs)

-- split a list into three parts according to a pivot element
class Split n xs ls eqs gs | n xs -> ls eqs gs
instance Split n Nil Nil Nil Nil
instance (Split n xs ls' eqs' gs',
        Cmp x n c,
    Pick c x ls' eqs' gs' ls eqs gs) =>
        Split n (Cons x xs) ls eqs gs

listSplit :: Split n xs ls eqs gs => (n, xs) -> (ls, eqs, gs)
listSplit = const (undefined, undefined, undefined)

-- zs = xs ++ ys
class App xs ys zs | xs ys -> zs
instance App Nil ys ys
instance App xs ys zs => App (Cons x xs) ys (Cons x zs)

-- zs = xs ++ [n] ++ ys
-- this is needed because
--
-- class CCons x xs xss | x xs -> xss
-- instance CCons x xs (Cons x xs)
--
-- doesn't work

class App' xs n ys zs | xs n ys -> zs
instance App' Nil n ys (Cons n ys)
instance (App' xs n ys zs) => App' (Cons x xs) n ys (Cons x zs)

-- quicksort
class QSort xs ys | xs -> ys
instance QSort Nil Nil
instance (Split x xs ls eqs gs,
        QSort ls ls',
    QSort gs gs',
    App eqs gs' geqs,
    App' ls' x geqs ys) =>
        QSort (Cons x xs) ys

listQSort :: QSort xs ys => xs -> ys
listQSort = const undefined

-- type-level lambda calculus

{-# OPTIONS -fglasgow-exts #-}
data X
data App t u
data Lam t

class Subst s t u | s t -> u
instance Subst X u u
instance (Subst s u s', Subst t u t') => Subst (App s t) u (App s' t')
instance Subst (Lam t) u (Lam t)

class Apply s t u | s t -> u
instance (Subst s t u, Eval u u') => Apply (Lam s) t u'

class Eval t u | t -> u
instance Eval X X
instance Eval (Lam t) (Lam t)
instance (Eval s s', Apply s' t u) => Eval (App s t) u

{-

Now, lets evaluate some lambda expressions:

 > :t undefined :: Eval (App (Lam X) X) u => u
 undefined :: Eval (App (Lam X) X) u => u :: X
Ok good, and:

 > :t undefined :: Eval (App (Lam (App X X)) (Lam (App X X)) ) u => u
 ^CInterrupted.

-}

-- It's possible to embed the Turing-complete SK combinator calculus at the type level.

