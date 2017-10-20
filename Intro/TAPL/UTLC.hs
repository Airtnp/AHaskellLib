data Term = 
    TmVar Int Int        -- a, context_length (represented by de Brujin index)
    | TmAbs String Term  -- \x -> M
    | TmApp Term Term    -- M N
    deriving(Show, Eq)

type Context = [String]

bindVarName :: String -> Context -> Context
bindVarName = (:)

getVarName :: Int -> Context -> String
getVarName n ctx
    | length ctx > n = ctx !! n
    | otherwise      = undefined

pkFreshVarName :: String -> Context -> (String, Context)
pkFreshVarName x ctx =
    let x' = mkFreshVarName x ctx in
        (x', bindVarName x' ctx)

mkFreshVarName :: String -> Context -> String
mkFreshVarName x [] = x
mkFreshVarName x ctx@(c:cs)
    | x == c    = mkFreshVarName (x ++ "'") ctx
    | otherwise = mkFreshVarName x cs

termPrint ctx t = case t of
    TmVar n _ -> getVarName n ctx
    TmAbs s t1 -> let (s', ctx') = pkFreshVarName s ctx in
        "(λ" ++ s' ++ " -> " ++ termPrint ctx' t1 ++ ")"
    TmApp t1 t2 -> 
        "(" ++ termPrint ctx t1 ++ " " ++ termPrint ctx t2 ++ ")"
    
-- [↑d, c]t
termShift d t = walk 0 t where
    walk :: Int -> Term -> Term
    walk c t' = case t' of
        TmVar i n   -> if i >= c then TmVar (i + d) (n + d) else TmVar i (n + d)
        TmAbs s t1  -> TmAbs s $ walk (c + 1) t1
        TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

-- [j -> s]t
termSubst j s t = walk 0 t where
    walk c t' = case t' of
        TmVar i n   -> if i == (j + c) then termShift c s else TmVar i n
        TmAbs s t1  -> TmAbs s $ walk (c + 1) t1
        TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)

-- beta reduction
termSubstTop s t = 
    termShift (-1) (termSubst 0 (termShift 1 s) t)

termIsVal (TmAbs _ _) = True
termIsVal _           = False

termEval1 :: Term -> Term
termEval1 (TmApp (TmAbs _ t12) v2) 
    | termIsVal v2 = termSubstTop v2 t12
termEval1 (TmApp t1 t2)
    | termIsVal t1 = TmApp t1 (termEval t2)
    | otherwise    = TmApp (termEval t1) t2
termEval1 t = t

fix :: (a -> a) -> a
fix f = let {x = f x} in x

termEval :: Term -> Term
termEval t = 
    let t' = termEval1 t in
        if t' == t then t else termEval t'

