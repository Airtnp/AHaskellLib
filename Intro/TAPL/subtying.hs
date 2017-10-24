-- ref: https://www.cis.upenn.edu/~bcpierce/tapl/checkers/fullfsub/syntax.ml
import Data.Maybe
import Data.List
data Type = 
    TyVar Int Int
    | TyId String
    | TyTop
    | TyBot
    | TyArr Type Type
    | TyRecord [(String, Type)]
    | TyVariant [(String, Type)]
    | TyRef Type
    | TyString
    | TyUnit
    | TyFloat
    | TyNat    
    | TyBool    
    | TyAll String Type Type
    | TySome String Type Type
    | TySource Type
    | TySink Type
    deriving(Eq, Show)

data Term = 
    TmVar Int Int                   -- Γ⊢a
    | TmAbs String Type Term        -- Γ⊢x:T.t1 (T->)
    | TmApp Term Term               -- Γ⊢M N
    | TmTrue
    | TmFalse
    | TmString String
    | TmUnit
    | TmFloat Float    
    | TmIf Term Term Term
    | TmRecord [(String, Term)]
    | TmProj Term String            -- record <-> proj
    | TmCase Term [(String, (String, Term))]
    | TmTag String Term Type
    | TmLet String Term Term        -- let x = t1 in t2 === (\x:T1 t2) t1 === [x -> t1]t2
    | TmFix Term                    -- fix \x:T1 t2 === [x -> fix \x:T1 t2]t2
    | TmAscribe Term Type           -- as
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
    | TmRef Term
    | TmDeref Term
    | TmAssign Term Term
    | TmError
    | TmTry Term Term
    | TmLoc Int
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
        TyBot                -> TyBot
        TyBool               -> TyBool
        TyRecord fs          -> TyRecord $ map (\lty -> (fst lty, walk c $ snd lty)) fs
        TyVariant fs         -> TyVariant $ map (\lty -> (fst lty, walk c $ snd lty)) fs
        TyString             -> TyString
        TyUnit               -> TyUnit
        TyFloat              -> TyFloat
        TyAll tyX tyT1 tyT2  -> TyAll tyX (walk c tyT1) (walk c tyT2)
        TyNat                -> TyNat
        TySome tyX tyT1 tyT2 -> TySome tyX (walk c tyT1) (walk c tyT2)
        TyRef tyT1           -> TyRef (walk c tyT1)
        TySource tyT1        -> TySource (walk c tyT1)
        TySink tyT1          -> TySink (walk c tyT1)

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
        TmTag l t1 tyT       -> TmTag l (walk c t1) (ontype c tyT)
        TmCase t cases       -> TmCase (walk c t) (map (\(li, (xi, ti)) -> (li, (xi, walk (c+1) ti))) cases)
        TmLoc l              -> TmLoc l
        TmRef t1             -> TmRef (walk c t1)
        TmDeref t1           -> TmDeref (walk c t1)
        TmAssign t1 t2       -> TmAssign (walk c t1) (walk c t2)
        TmError              -> TmError
        TmTry t1 t2          -> TmTry (walk c t1) (walk c t2)


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
small t = case t of
    TmVar _ _ -> True
    _         -> False

-- TODO: printType / printTerm

-- Evaluation
isNumericVal ctx t = case t of
    TmZero    -> True
    TmSucc t1 -> isNumericVal ctx t1
    _         -> False

isVal ctx t = case t of
    TmTrue                 -> True
    TmFalse                -> True
    TmString _             -> True
    TmUnit                 -> True
    TmFloat _              -> True
    TmLoc _                -> True
    t | isNumericVal ctx t -> True
    TmAbs _ _ _            -> True
    TmRecord fs            -> all (\(l,ti) -> isVal ctx ti) fs
    TmPack _ v1 _          -> isVal ctx v1
    TmTAbs _ _ _           -> True
    _                      -> False

type Store = [Term]

extendStore store v = (length store, store ++ [v])

lookupLoc store l = store !! l

updateStore store n v = f (n, store) where
    f s = case s of
            (0, v':rest) -> v:rest
            (n, v':rest) -> v':f(n-1, rest)
            _            -> error "updateStore: bad index"

shiftStore i store = map (\t -> termShift i t) store

eval1 ctx store t = case t of
    TmIf TmTrue t2 t3  -> (t2, store)
    TmIf TmFalse t2 t3 -> (t3, store)
    TmIf t1 t2 t3      -> let (t1', store') = eval1 ctx store t1 in
        (TmIf t1' t2 t3, store')
    TmRecord fs -> 
        let evalField l = case l of
                            [] -> (error "No Rule Applies")
                            (l, vi):rest 
                                | isVal ctx vi -> let (rest', store') = evalField rest in
                                    ((l, vi):rest', store')
                                | otherwise    -> let (ti', store') = eval1 ctx store vi in
                                    ((l, ti'):rest, store')
        in let (fs', store') = evalField fs in 
            (TmRecord fs', store')
    TmProj (TmRecord fs) l
        | isVal ctx (TmRecord fs) -> let rd = filter (\(li, ti) -> li == l) fs in
            if rd == [] then (error "No Rule Applies") else (snd $ rd !! 0, store)
    TmProj t1 l                   -> let (t1', store') = eval1 ctx store t1 in
            (TmProj t1' l, store')
    TmLet x v1 t2
        | isVal ctx v1 -> (termSubstTop v1 t2, store)
        | otherwise    -> let (t1', store') = eval1 ctx store v1 in
            (TmLet x t1' t2, store')
    TmFix v1
        | isVal ctx v1 -> case v1 of
            TmAbs _ _ t12 -> (termSubstTop (TmFix v1) t12, store)
            _             -> (error "No Rule Applies")
        | otherwise    -> let (t1', store') = eval1 ctx store v1 in
            (TmFix t1', store')
    TmTag l t1 tyT -> let (t1', store') = eval1 ctx store t1 in
        (TmTag l t1' tyT, store')
    TmCase (TmTag li v11 tyT) branches
        | isVal ctx v11 -> let rd = filter (\(li', (xi, ti)) -> li == li') branches in
            if rd == [] then (error "No Rule Applies") else (termSubstTop v11 $ snd $ snd (rd !! 0), store)
    TmCase t1 branches -> let (t1', store') = eval1 ctx store t1 in
        (TmCase t1' branches, store')
    TmAscribe v1 tyT
        | isVal ctx v1 -> (v1, store)
        | otherwise    -> let (t1', store') = eval1 ctx store v1 in
            (TmAscribe t1' tyT, store')
    TmVar n _ -> case getBinding ctx n of
        TmAbbBinding t _ -> (t, store)
        _                -> (error "No Rule Applies")
    TmRef t1
        | isVal ctx t1 -> let (l, store') = extendStore store t1 in
            (TmLoc l, store')
        | otherwise    -> let (t1', store') = eval1 ctx store t1 in
            (TmRef t1', store')
    TmDeref t1
        | isVal ctx t1 -> case t1 of
            TmLoc l -> (lookupLoc store l, store)
            _       -> (error "No Rule Applies")
        | otherwise    -> let (t1', store') = eval1 ctx store t1 in
            (TmDeref t1', store')
    TmAssign t1 t2
        | (isVal ctx t1) && (isVal ctx t2) -> case t1 of
            TmLoc l -> (TmUnit, updateStore store l t2)
            _       -> (error "No Rule Applies")
        | not (isVal ctx t1)               -> let (t1', store') = eval1 ctx store t1 in
            (TmAssign t1' t2, store')
        | not (isVal ctx t2)               -> let (t2', store') = eval1 ctx store t2 in
            (TmAssign t1 t2', store')
    TmError -> (error "Error Encountered")
    TmTimesFloat (TmFloat f1) (TmFloat f2) -> (TmFloat (f1*f2), store)
    TmTimesFloat (TmFloat f1) t2           -> let (t2', store') = eval1 ctx store t2 in
        (TmTimesFloat (TmFloat f1) t2', store')
    TmTimesFloat t1 t2                     -> let (t1', store') = eval1 ctx store t1 in
        (TmTimesFloat t1' t2, store')
    TmSucc t1 -> let (t1', store') = eval1 ctx store t1 in
        (TmSucc t1', store')
    TmPred t1 -> let (t1', store') = eval1 ctx store t1 in
        (TmPred t1', store')
    TmIsZero TmZero            -> (TmTrue, store)
    TmIsZero (TmSucc nv1)
        | isNumericVal ctx nv1 -> (TmFalse, store)
    TmIsZero t1                -> let (t1', store') = eval1 ctx store t1 in
        (TmIsZero t1', store')
    TmTApp (TmTAbs x _ t11) tyT2 -> (tyTermSubstTop tyT2 t11, store)
    TmTApp t1 tyT2               -> let (t1', store') = eval1 ctx store t1 in
        (TmTApp t1' tyT2, store')
    TmApp (TmAbs x tyT11 t12) v2
        | isVal ctx v2 -> (termSubstTop v2 t12, store)
    TmApp v1 t2
        | isVal ctx v1 -> let (t2', store') = eval1 ctx store t2 in
            (TmApp v1 t2', store')
    TmApp t1 t2 -> let (t1', store') = eval1 ctx store t2 in
        (TmApp t1' t2, store')
    TmPack tyT1 t2 tyT3 -> let (t2', store') = eval1 ctx store t2 in
        (TmPack tyT1 t2' tyT3, store')
    TmUnpack _ _ (TmPack tyT11 v12 _) t2
        | isVal ctx v12 -> (tyTermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2), store)
    TmUnpack tyX x t1 t2 -> let (t1', store') = eval1 ctx store t1 in
        (TmUnpack tyX x t1' t2, store')

eval ctx store t = let (t', store') = eval1 ctx store t in
    if (t', store') == (t, store) 
        then (t, store) 
        else eval ctx store' t'

