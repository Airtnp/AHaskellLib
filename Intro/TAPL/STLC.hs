import Data.List

data Binding = 
    NameBinding
    | VarBinding Type
    deriving(Eq, Show)

type Context = [(String, Binding)]

data Type =
    TyUnit
    | TyBool
    | TyNat
    | TyProduct [Type]             -- A x B
    | TyArr Type Type              -- T -> T
    | TyError
    deriving(Eq)
    -- TyRecord [Type]
    -- TyVariant [Type]            -- A | B
    -- TyList Type

instance Show Type where
    show TyUnit = "Unit"
    show TyBool = "Bool"
    show TyNat  = "Nat"
    show (TyProduct tys) = "{" ++ (concat . intersperse ", " . map show) tys ++ "}"
    show (TyArr ty1 ty2) = show ty1 ++ " -> " ++ show ty2
    show TyError = "Error"

data Term = 
    TmUnit
    | TmAscription Term Type       -- as
    | TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmVar Int Int                -- Γ⊢a
    | TmAbs String Type Term       -- Γ⊢x:T.t1
    | TmApp Term Term              -- Γ⊢M N (also acts as let)
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    | TmProduct [Term]              -- tuple
    | TmProj Int Term               -- project
    deriving (Eq, Show)
    -- TmRecord [(String, Term)]
    -- TmRecProj String Term
    -- TmVariant Int Term           -- eg. Option<T> = {none: Unit, some: T}
    -- TmFix Term                   -- [x -> fix(\x:T.t2)]t2
    -- TmNil
    -- TmCons Term Term

data TypeError = 
    NoError
    | AscribedTypeExpected Type
    | IfArmsTypeMismatch
    | IfGuardBoolTypeExpected
    | SuccArgNatTypeExpected
    | PredArgNatTypeExpected
    | IsZeroArgNatTypeExpected
    | ProductWellTypeExpected
    | ProjProductTypeExpected
    | ArrowParamTypeMismatch
    | AppOpArrowTypeExpected
    | VarTypeErrorWat
    deriving(Eq, Show) 

addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

getBinding :: Context -> String -> Binding
getBinding ctx x = case lookup x ctx of
    Just (VarBinding ty) -> VarBinding ty
    _                    -> undefined

getTypeFromContext ctx i = case getBinding ctx i of
    VarBinding ty -> ty
    _             -> undefined

getVarName :: Context -> Int -> String
getVarName ctx i = fst $ ctx !! i

getType :: Context -> Int -> Binding
getType ctx i = snd $ ctx !! i

pkFreshVarName :: Context -> String -> (String, Context)
pkFreshVarName ctx x =
    let x' = mkFreshVarName ctx x in
        (x', addBinding ctx x' NameBinding)

mkFreshVarName :: Context -> String -> String
mkFreshVarName [] x = x
mkFreshVarName ctx@(c:cs) x
    | x == (fst c)    = mkFreshVarName ctx (x ++ "'")
    | otherwise       = mkFreshVarName cs x

-- Typecheck rule
typeOf :: Context -> Term -> (TypeError, Type)
typeOf ctx t = case t of
    TmUnit                  -> (NoError, TyUnit)
    TmAscription t1 ty1     -> 
        if (snd $ typeOf ctx t1) == ty1
            then (NoError, ty1)
            else (AscribedTypeExpected ty1, TyError)
    TmTrue                  -> (NoError, TyBool)
    TmFalse                 -> (NoError, TyBool)
    TmIf t1 t2 t3           -> 
        case snd $ typeOf ctx t1 of
            TyBool -> let ty2 = snd $ typeOf ctx t2
                          ty3 = snd $ typeOf ctx t3          
                      in if ty2 == ty3
                            then (NoError, ty2)
                            else (IfArmsTypeMismatch, TyError)
            _      -> (IfGuardBoolTypeExpected, TyError)
    TmZero                  -> (NoError, TyNat)
    TmSucc t1               -> 
        if (snd $ typeOf ctx t1) == TyNat
            then (NoError, TyNat)
            else (SuccArgNatTypeExpected, TyError)
    TmPred t1               -> 
        if (snd $ typeOf ctx t1) == TyNat
            then (NoError, TyNat)
            else (PredArgNatTypeExpected, TyError)
    TmIsZero t1             -> 
        if (snd $ typeOf ctx t1) == TyNat
            then (NoError, TyNat)
            else (IsZeroArgNatTypeExpected, TyError)
    TmProduct ts            -> 
        let tys = map (snd . typeOf ctx) ts in
            if elem TyError tys
                then (ProductWellTypeExpected, TyError)     -- discard the error list ?
                else (NoError, TyProduct tys)
    TmProj x t1             -> case (snd $ typeOf ctx t1) of
        TyProduct tys -> (NoError, tys !! x)
        _             -> (ProjProductTypeExpected, TyError) -- discard the nested error ?
    TmVar i _               -> case getType ctx i of
        VarBinding ty -> (NoError, ty)
        _             -> (VarTypeErrorWat, TyError)
    TmAbs x ty1 t2          ->
        let ctx' = addBinding ctx x (VarBinding ty1) in
            case snd $ typeOf ctx' t2 of
                TyError     -> typeOf ctx' t2
                _           -> (NoError, TyArr ty1 (snd $ typeOf ctx' t2))
    TmApp t1 t2             ->
        let ty1 = (snd $ typeOf ctx t1) 
            ty2 = snd $ typeOf ctx t2    
        in case ty1 of
            TyArr ty1' ty2' -> 
                if ty2 == ty1' 
                    then (NoError, ty2')
                    else (ArrowParamTypeMismatch, TyError)
            _               -> (AppOpArrowTypeExpected, TyError)


-- Eval rule
shiftTerm :: Int -> Term -> Term
shiftTerm d t = walk 0 t where 
    walk c (TmIf t1 t2 t3)     = TmIf (walk c t1) (walk c t2) (walk c t3)
    walk c (TmVar x n)
        | x >= c    = TmVar (x + d) (n + d)
        | otherwise = TmVar x (n + d)
    walk c (TmAbs x ty1 t2)    = TmAbs x ty1 (walk (c + 1) t2)
    walk c (TmApp t1 t2)       = TmApp (walk c t1) (walk c t2)
    walk _ t                   = t

substTerm :: Int -> Term -> Term -> Term
substTerm j s t = walk 0 t where 
    walk c (TmAscription t1 tyT1) = TmAscription (walk c t1) tyT1
    walk c (TmIf t1 t2 t3)        = TmIf (walk c t1) (walk c t2) (walk c t3)
    walk c (TmSucc t1)            = TmSucc (walk c t1)
    walk c (TmPred t1)            = TmPred (walk c t1)
    walk c (TmIsZero t1)          = TmIsZero (walk c t1)
    walk c (TmProduct ts)         = TmProduct (map (walk c) ts)
    walk c (TmProj x t1)          = TmProj x (walk c t1)
    walk c (TmVar x n)
        | x == j+c  = s
        | otherwise = TmVar x n
    walk c (TmAbs x ty1 t2)       = TmAbs x ty1 (walk (c + 1) t2)
    walk c (TmApp t1 t2)          = TmApp (walk c t1) (walk c t2)
    walk _ t1
        | t1 == TmUnit  = t1
        | t1 == TmTrue  = t1
        | t1 == TmFalse = t1
        | t1 == TmZero  = t1
        | otherwise     = s

substTopTerm :: Term -> Term -> Term
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

isValue :: Term -> Bool
isValue TmUnit                 = True
isValue TmTrue                 = True
isValue TmFalse                = True
isValue TmZero                 = True
isValue (TmSucc (TmPred t1))   = False
isValue (TmSucc t1)            = isValue t1
isValue (TmPred (TmSucc t1))   = False
isValue (TmPred t1)            = isValue t1
isValue (TmProduct ts)         = (and . map isValue) ts
isValue (TmAbs _ _ _)          = True
isValue _                      = False

eval1 :: Term -> Term
eval1 (TmAscription t1 tyT1)       = t1
eval1 (TmIf TmTrue  t2 _ )         = t2
eval1 (TmIf TmFalse _  t3)         = t3
eval1 (TmIf t1 t2 t3)              = (\t1' -> TmIf t1' t2 t3) (eval1 t1)
eval1 (TmSucc t1)                  = TmSucc (eval1 t1)
eval1 (TmPred TmZero)              = TmZero
eval1 (TmPred (TmSucc t1))         = t1
eval1 (TmPred t1)                  = TmPred (eval1 t1)
eval1 (TmIsZero TmZero)            = TmTrue
eval1 (TmIsZero (TmSucc TmZero))   = TmFalse
eval1 t@(TmProduct ts)
    | isValue t = t
    | otherwise = TmProduct $ 
                    (map 
                        (\t -> if isValue t then t else eval1 t)
                    ts)
eval1 (TmProj x (TmProduct ts))    = ts !! x
eval1 (TmProj x t1)                = TmProj x (eval1 t1)
eval1 (TmIsZero t)                 = TmIsZero (eval1 t)
eval1 (TmApp (TmAbs _ _ t12) v2)
    | isValue v2 = substTopTerm v2 t12
eval1 (TmApp t1 t2)
    | isValue t1 = TmApp t1 (eval1 t2)
    | otherwise  = TmApp (eval1 t1) t2
eval1 t                            = t

eval :: Term -> Term
eval t = 
    let t' = eval1 t in
        if t' == t then t else eval t'
