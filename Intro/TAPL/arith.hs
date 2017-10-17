data Term = 
    TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    deriving (Show)

isnumericval :: Term -> Bool
isnumericval t = 
    case t of
        TmZero -> True
        TmSucc t1 -> isnumericval t1
        _ -> False

isval :: Term -> Bool
isval t = 
    case t of
        TmTrue -> True
        TmFalse -> True
        _ -> isnumericval t

-- small step
eval :: Term -> Term
eval t = 
    case t of 
        TmIf TmTrue t2 t3 -> t2
        TmIf TmFalse t2 t3 -> t3
        TmIf t1 t2 t3 -> let t1' = eval t1 in
            TmIf t1' t2 t3
        TmSucc t1 -> let t1' = eval t1 in
            TmSucc t1'
        TmPred TmZero -> TmZero
        TmPred (TmSucc t1) | isnumericval t1 -> t1
        TmPred t1 -> let t1' = eval t1 in
            TmPred t1'
        TmIsZero TmZero -> TmTrue
        TmIsZero (TmSucc t1) | isnumericval t1 -> TmFalse
        TmIsZero t1 -> let t1' = eval t1 in
            TmIsZero t1'
        TmZero -> TmZero
        TmTrue -> TmTrue
        TmFalse -> TmFalse

