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
    | TyProduct [Type]
    | TyArr Type Type              -- T -> T
    | TyError
    deriving(Eq)

instance Show Type where
    show TyUnit = "Unit"
    show TyBool = "Bool"
    show TyNat  = "Nat"
    show (TyProduct tys) = "{" ++ (concat . intersperse ", " . map show) tys ++ "}"
    show (TyArr ty1 ty2) = show ty1 ++ " -> " ++ show ty2
    show TyError = "Error"

data Term = 
    TmUnit
    | TmAscription Term Type
    | TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmVar Int Int                -- Γ⊢a
    | TmAbs String Type Term       -- Γ⊢x:T.t1
    | TmApp Term Term              -- Γ⊢M N
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    | TmProduct [Term]
    | TmProj Int Term
    deriving (Eq, Show)

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


        

