import Data.Maybe

data Type = 
    TyVar Int Int
    | TyId String
    | TyTop
    | TyArr Type Type
    | TyBool
    | TyRecord [(String, Type)]
    | TyString
    | TyUnit
    | TyFloat
    | TyAll String Type Type
    | TyNat
    | TySome String Type Type
    deriving(Eq, Show)

data Term = 
    TmVar Int Int                   -- Γ⊢a
    | TmAbs String Type Term        -- Γ⊢x:T.t1 (T->)
    | TmApp Term Term               -- Γ⊢M N
    | TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmRecord [(String, Term)]
    | TmProj Term String            -- record <-> proj
    | TmLet String Term Term        -- function(abstraction) def
    | TmFix Term                    -- fix
    | TmString String
    | TmUnit
    | TmAscribe Term Type           -- as
    | TmFloat Float
    | TmTimesFloat Term Term
    | TmTAbs String Type Term
    | TmTApp Term Type
    | TmZero
    | TmSucc Term 
    | TmPred Term
    | TmIsZero Term
    | TmInert Type
    | TmPack Type Term Type
    | TmUnpack String String Term Term
    deriving(Eq, Show)
    
data Binding = 
    NameBinding
    | TyVarBinding Type
    | VarBinding Type
    | TyAbbBinding Type
    | TmAbbBinding Term (Maybe Type)
    deriving(Eq, Show)
    
type Context = [(String, Binding)]

data Command = 
    Eval Term 
    | Bind String Binding
    | SomeBind String String Term

-- Context
addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

addName :: Context -> String -> Context
addName ctx x = addBinding ctx x NameBinding

isNameBound :: Context -> String -> Bool
isNameBound ctx x = case ctx of
    []   -> False
    y:ys -> if x == (fst y) then True else isNameBound ys x

pkFreshVarName :: Context -> String -> (String, Context)
pkFreshVarName ctx x =
    let x' = mkFreshVarName ctx x in
        (x', addBinding ctx x' NameBinding)

mkFreshVarName :: Context -> String -> String
mkFreshVarName [] x = x
mkFreshVarName ctx@(c:cs) x
    | x == (fst c)    = mkFreshVarName ctx (x ++ "'")
    | otherwise       = mkFreshVarName cs x

indexToName :: Context -> Int -> String
indexToName ctx i
    | length ctx >= i = fst $ ctx !! i
    | otherwise       = undefined

nameToIndex :: Context -> String -> Int
nameToIndex ctx x = case ctx of
    []   -> undefined
    y:ys -> if x == (fst y) then 0 else (+) 1 $ nameToIndex ctx x

-- Shifting
-- instance Functor Type where
tyMap onvar c tyT = walk c tyT where
    walk c tyT = case tyT of
        TyVar x n            -> onvar c x n
        TyId b               -> TyId b
        TyArr tyT1 tyT2      -> TyArr (walk c tyT1) (walk c tyT2)
        TyTop                -> TyTop
        TyBool               -> TyBool
        TyRecord fs          -> TyRecord $ map (\lty -> (fst lty, walk c $ snd lty)) fs
        TyString             -> TyString
        TyUnit               -> TyUnit
        TyFloat              -> TyFloat
        TyAll tyX tyT1 tyT2  -> TyAll tyX (walk c tyT1) (walk c tyT2)
        TyNat                -> TyNat
        TySome tyX tyT1 tyT2 -> TySome tyX (walk c tyT1) (walk c tyT2)

tmMap onvar ontype c t = walk c t where
    walk c t = case t of
        TmInert tyT          -> TmInert $ ontype c tyT
        TmVar x n            -> onvar c x n
        TmAbs x tyT1 t2      -> TmAbs x (ontype c tyT1) (walk (c+1) t2)
        TmApp t1 t2          -> TmApp (walk c t1) (walk c t2)
        TmTrue               -> TmTrue
        TmFalse              -> TmFalse
        TmIf t1 t2 t3        -> TmIf (walk c t1) (walk c t2) (walk c t3)
        TmProj t1 l          -> TmProj (walk c t1) l
        TmRecord fs          -> TmRecord $ map (\lt -> (fst lt, walk c $ snd lt)) fs
        TmLet x t1 t2        -> TmLet x (walk c t1) (walk (c+1) t2)
        TmFix t1             -> TmFix (walk c t1)
        TmString x           -> TmString x
        TmUnit               -> TmUnit
        TmAscribe t1 tyT1    -> TmAscribe (walk c t1) (ontype c tyT1)
        TmFloat x            -> TmFloat x
        TmTimesFloat t1 t2   -> TmTimesFloat (walk c t1) (walk c t2)
        TmTAbs tyX tyT1 t2   -> TmTAbs tyX (ontype c tyT1) (walk (c+1) t2)
        TmTApp t1 tyT2       -> TmTApp (walk c t1) (ontype c tyT2)
        TmZero               -> TmZero
        TmSucc t1            -> TmSucc (walk c t1)
        TmPred t1            -> TmPred (walk c t1)
        TmIsZero t1          -> TmIsZero (walk c t1)
        TmPack tyT1 t2 tyT3  -> TmPack (ontype c tyT1) (walk c t2) (ontype c tyT3)
        TmUnpack tyX x t1 t2 -> TmUnpack tyX x (walk c t1) (walk (c+2) t2)

typeShiftAbove d c tyT = 
    tyMap 
        (\c x n -> if x >= c then TyVar (x+d) (n+d) else TyVar x (n+d))
        c tyT

-- [↑d, c]t
termShiftAbove d c t = 
    tmMap
        (\c' x n -> if x >= c' then TmVar (x+d) (n+d) else TmVar x (n+d))
        (typeShiftAbove d)
        c t

typeShift d tyT = typeShiftAbove d 0 tyT

termShift d t = termShiftAbove d 0 t

bindingShift d bind = case bind of
    NameBinding -> NameBinding
    TyVarBinding tyS -> TyVarBinding (typeShift d tyS)
    VarBinding tyT -> VarBinding (typeShift d tyT)
    TyAbbBinding tyT -> TyAbbBinding (typeShift d tyT)
    TmAbbBinding t tyT_opt -> 
        let tyT_opt' = case tyT_opt of
                        Nothing  -> Nothing
                        Just tyT -> Just (typeShift d tyT)
        in TmAbbBinding (termShift d t) tyT_opt'

-- Substitution
-- [j -> s]t
termSubst j s t =
    tmMap
        (\j x n -> if x == j then termShift j s else TmVar x n)
        (\j tyT -> tyT)
        j t

typeSubst tyS j tyT =
    tyMap
        (\j x n -> if x == j then typeShift j tyS else TyVar x n)
        j tyT

termSubstTop s t = 
    termShift (-1) (termSubst 0 (termShift 1 s) t)

typeSubstTop tyS tyT = 
    typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

tyTermSubst tyS j t =
    tmMap
        (\c x n -> TmVar x n)
        (\j tyT -> typeSubst tyS j tyT)
        j t

tyTermSubstTop tyS t =
    termShift (-1) (tyTermSubst (typeShift 1 tyS) 0 t)

-- Context
getBinding ctx i
    | length ctx >= i = let bind = snd $ ctx !! i
                        in bindingShift (i + 1) bind
    | otherwise       = undefined

getTypeFromContext ctx i = 
    case getBinding ctx i of
        VarBinding tyT            -> tyT
        TmAbbBinding _ (Just tyT) -> tyT
        TmAbbBinding _ Nothing    -> undefined
        _                         -> undefined

-- Print
