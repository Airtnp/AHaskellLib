-- Recursion Schemes
{-

Name		
Catamorphism	
    foldr :: (a -> b -> b) -> b -> [a] -> b	
    Deconstructs a data structure
Anamorphism	
    unfoldr :: (b -> Maybe (a, b)) -> b -> [a]	
    Constructs a structure level by level

-}

-- | A fix-point type. 
newtype Fix f = Fix { unFix :: f (Fix f) }

-- | Catamorphism or generic function fold. 
cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

-- | Anamorphism or generic function unfold. 
ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Functor.Foldable

type Var = String

data Exp
  = Var Var
  | App Exp Exp
  | Lam [Var] Exp
  deriving Show

data ExpF a
  = VarF Var
  | AppF a a
  | LamF [Var] a
  deriving Functor

type instance Base Exp = ExpF

instance Foldable Exp where
  project (Var a)     = VarF a
  project (App a b)   = AppF a b
  project (Lam a b)   = LamF a b

instance Unfoldable Exp where
  embed (VarF a)      = Var a
  embed (AppF a b)    = App a b
  embed (LamF a b)    = Lam a b

fvs :: Exp -> [Var]
fvs = cata phi
  where phi (VarF a)    = [a]
        phi (AppF a b)  = a ++ b
        phi (LamF a b) = foldr (filter . (/=)) a b

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Traversable
import Control.Monad hiding (forM_, mapM, sequence)
import Prelude hiding (mapM)
import qualified Data.Map as M

newtype Fix (f :: * -> *) = Fix { outF :: f (Fix f) }

-- Catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . outF

-- Monadic catamorphism
cataM :: (Traversable f, Monad m) => (f a -> m a) -> Fix f -> m a
cataM f = f <=< mapM (cataM f) . outF

data ExprF r
  = EVar String
  | EApp r r
  | ELam r r
  deriving (Show, Eq, Ord, Functor)

type Expr = Fix ExprF

instance Show (Fix ExprF) where
  show (Fix f) = show f

instance Eq (Fix ExprF) where
  Fix x == Fix y = x == y

instance Ord (Fix ExprF) where
  compare (Fix x) (Fix y) = compare x y


mkApp :: Fix ExprF -> Fix ExprF -> Fix ExprF
mkApp x y = Fix (EApp x y)

mkVar :: String -> Fix ExprF
mkVar x = Fix (EVar x)

mkLam :: Fix ExprF -> Fix ExprF -> Fix ExprF
mkLam x y = Fix (ELam x y)

i :: Fix ExprF
i = mkLam (mkVar "x") (mkVar "x")

k :: Fix ExprF
k = mkLam (mkVar "x") $ mkLam (mkVar "y") $ (mkVar "x")

subst :: M.Map String (ExprF Expr) -> Expr -> Expr
subst env = cata alg where
  alg (EVar x) | Just e <- M.lookup x env = Fix e
  alg e = Fix e