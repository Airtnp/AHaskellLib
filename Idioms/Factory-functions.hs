-- Factory functions
-- ref: https://wiki.haskell.org/Factory_function

-- If you need more intelligence from your constructor functions, use a real function instead. Also known as smart constructors.

-- Consider the following data type:

data Expr = EAdd Expr Expr | EMult Expr Expr | EInt Int | EVar String

-- Keeping an expression in a relatively simplified form can be difficult if it is modified a lot. One simple way is to write replacements for the constructor functions:

eInt i = EInt i
 
eAdd (EInt i1) (EInt i2) = eInt (i1+i2)
eAdd (EInt 0)  e2        = e2
eAdd e1        (EInt 0)  = e1
eAdd e1        e2        = EAdd e1 e2
 
eMult (EInt 0) e2        = eInt 0
{- etc -}

-- Then if you need to construct an expression, use the factory functions:

derivative :: String -> Expr -> Expr
derivative x (EMult e1 e2)
  = eAdd (eMult (derivative x e1) e2) (eMult e1 (derivative x e2))
{- etc -}

-- RB-Tree

-- The children of a red node are black.
-- There are the same number of black nodes on every path from root to leaf.

data Colour = R | B
	      deriving (Eq, Show, Ord)
 
data RBSet a = Empty |
	     RBTip Colour (RBSet a) a (RBSet a)
		   deriving Show

empty :: RBSet a
empty = Empty

member :: Ord a => a -> RBSet a -> Bool
member _ Empty = False
member x (RBTip _ a y b)
    | x < y = member x a
    | x == y = True
    | x > y = member x b

insert :: Ord a => a -> RBSet a -> RBSet a
insert x s = makeBlack (ins s) where
    ins Empty = RBTip R Empty x Empty
    ins (RBTip color a y b)
        | x < y = balance color (ins a) y b
        | x == y = RBTip color a y b
        | x > y =  balance color a y (ins b)
    makeBlack (RBTip _ a y b) = RBTip B a y b

-- To ensure invariants

balance :: Colour -> RBSet a -> a -> RBSet a -> RBSet a
-- color flips
balance B (RBTip R (RBTip R a x b) y c) z d 
	= RBTip R (RBTip B a x b) y (RBTip B c z d)
-- single rotations
balance B (RBTip R a x (RBTip R b y c)) z d 
	= RBTip R (RBTip B a x b) y (RBTip B c z d)
balance B a x (RBTip R (RBTip R b y c) z d) 
	= RBTip R (RBTip B a x b) y (RBTip B c z d)
-- double rotations
balance B a x (RBTip R b y (RBTip R  c z d)) 
	= RBTip R (RBTip B a x b) y (RBTip B c z d)
-- no balancing necessary
balance c a x b = RBTip c a x b