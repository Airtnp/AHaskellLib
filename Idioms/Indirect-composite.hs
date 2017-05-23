-- Indirect composite
-- ref: https://wiki.haskell.org/Indirect_composite
-- paper: Two-level types and parameterized modules

--  For example, you might want one version with decorating structures and one without, or you might want to use hash consing to build your data structures. However, you don't want to duplicate constructors for all of the different versions of what is essentially the same data structure.

-- Take, for example, this simple lambda calculus type :

data Expr
    = EApp Expr Expr
    | EVar String
    | ELambda String Expr
    | ELet String Expr Expr

-- An indirectly recursive version is this:

data Expr' expr
    = EApp expr expr
    | EVar String 
    | ELambda String expr
    | ELet String expr expr

newtype Expr = Expr (Expr' Expr)

-- Alternative version which uses a type-level [[fixed point combinator]]
newtype Fix f = In { out :: f (Fix f) }
 
type Expr2 = Fix Expr'

-- You can then produce a version with decorations:

data DExpr d
    = DExpr d (Expr' (DExpr d))

-- or a mutable version which uses IORefs:

newtype IOExpr
    = IOExpr (Expr' (IORef IOExpr))

-- or a non-recursive version ready for hash consing:

type HCExpr
    = Expr' Int