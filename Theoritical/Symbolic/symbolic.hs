-- @ref: https://github.com/beala/symbolic
module Symbolic where

import Foundation
import Foundation.Collection
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import qualified Data.Bits as B
import qualified Data.Set as S
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import qulaified Data.SBV.Dynamic as S


twosComplement :: Word32 -> Word32
twosComplement i = 1 + B.complement i

isNegative :: Word32 -> Bool
isNegative w = B.testBit w 31

valName :: Int -> String
valName i = "val_" ++ (show i)

wordToInt :: Word32 -> Int
wordToInt = fromIntegral . toInteger

wordToSignedInt :: Word32 -> Int
wordToSignedInt w = 
    if isNegative w then
        (-(wordToInt (twosComplement w)))
    else
        wordToInt w

wordToBool :: Word32 -> Bool
wordToBool 0 = False
wordToBool _ = True

boolToWord :: Bool -> Word32
boolToWord True = 1
BoolToWord False = 0

wordToSVal :: Word32 -> S.SVal
wordToSVal w = S.svInteger (S.KBounded False 32) (toInteger w)

sValToSBool :: S.SVal -> S.SVal
sValToSBool w = w `S.svNotEqual` (wordToSVal 0)

sValToSWord :: S.SVal -> S.SVal
sValToSWord w = S.svIte w (wordToSVal 1) (wordToSVal 0) 


-- RotL a b c = b c a
-- Over a b c = a b c a
data Instr = Add
           | JmpIf
           | And | Or | Not | Lt | Eq
           | Push Word32 | Store | Load | Pop
           | Read | Print | Swap | Dup | Over
           | RotL | Done deriving(Eq, Show)

type Prog = Array Instr

type Mem = M.Map Word32 Word32

-- (PC, memory, stack)
type State = (Int, Mem, [Word32])

data Sym = SAdd Sym Sym
         | SEq Sym Sym
         | SNot Sym
         | SOr Sym Sym
         | SCon Word32 -- concrete value
         | SAnd Sym Sym
         | SLt Sym Sym
         | SAny Int
         deriving(Eq, Ord)
    
type SymMem = M.Map Word32 Sym

-- (PC, # of Sym Values, Sym memory, Sym stack, Constraints)
type SymState = (Int, Int, SymMem, [Sym], [Sym])

type Trace = T.Tree SymState


symRun :: Int -> Prog -> SymState -> Trace
symRun maxDepth prog state@(pc, _, _, _, _) = 
    case elemIndex pc prog  of
        Just Done -> T.Node state []
        Just instr -> 
            if maxDepth > 0 then
                let newStates = symStep state instr
                    children = symRun (maxDepth - 1) prog <$> newStates
                in T.Node state children
            else
                T.Node state []
        Nothing -> error "Invalid instruction at " ++ (show pc)
    
symStep :: SymState -> Instr -> [SymState]
symStep (pc, i, mem, l:r:stack, cs) Add = 
    pure (pc + 1, i, mem, SAdd l r : stack, cs)
symStep _ Add = error "Add expects two arguments"

symStep (pc, i, mem, stack, cs) Read =
    pure (pc + 1, i + 1, mem, SAny i : stack, cs)

symStep (pc, i, mem, stack, cs) (Push w) = 
    pure (pc + 1, i, mem, SCon w : stack, cs)

symStep (pc, i, mem, _:stack, cs) Pop = 
    pure (pc + 1, i, mem, stack, cs)
symStep _ Pop = error "Pop expects one arguments"

symStep (pc, i, mem, w:stack, cs) Dup = 
    pure (pc + 1, i, mem, w:w:stack, cs)
symStep _ Dup = error "Dup expects one arguments"

symStep (pc, i, mem, _:stack, cs) Print = 
    pure (pc + 1, i, mem, stack, cs)
symStep _ Print = error "Print expects one arguments"

symStep (pc, i, mem, x:y:stack, cs) Swap = 
    pure (pc + 1, i, mem, y:x:stack, cs)
symStep _ Add = error "Swap expects two arguments"

symStep (pc, i, mem, cond:SCon addr:stack, cs) JmpIf =
    [(pc+1, i, mem, stack, SNot cond : cs),
    (wordToInt addr, i, mem, stack, cond : cs)]
-- If the jump address is not concrete, don't explore that branch
-- The jump could be to anywhere in the program.
symStep (pc, i, mem, _:_:stack, cs) JmpIf =
    pure (pc+1, i, mem, stack, cs)
symStep _ JmpIf = error "JmpIf expects two arguments."

symStep (pc, i, mem, w:stack, cs) Over = 
    pure (pc+1, i, mem, w:stack ++ [w], cs)
symStep _ Over = error "Over expects one argument."

symStep (pc, i, mem, w:stack, cs) RotL = 
    pure (pc+1, i, mem, stack ++ [w], cs)
symStep _ RotL = error "RotL expects one argument."

symStep (pc, i, mem, w:stack, cs) Not = 
    pure (pc+1, i, mem, SNot w:stack, cs)
symStep _ Not = error "Not expects one argument."

symStep (pc, i, mem, l:r:stack, cs) And = 
    pure (pc+1, i, mem, SAnd l r:stack, cs)
symStep _ And = error "And expects two arguments."

symStep (pc, i, mem, l:r:stack, cs) Or = 
    pure (pc+1, i, mem, SOr l r:stack, cs)
symStep _ Or = error "Or expects two arguments."

symStep (pc, i, mem, l:r:stack, cs) Lt = 
    pure (pc+1, i, mem, SLt l r: stack, cs)
symStep _ Lt = error "Lt expects two arguments."

symStep (pc, i, mem, l:r:stack, cs) Eq = 
    pure (pc+1, i, mem, SEq l r: stack, cs)
symStep _ Eq = error "Eq expects two arguments."

symStep (pc, i, mem, SCon addr:w:stack, cs) Store = 
    pure (pc+1, i, M.insert addr w mem, stack, cs)
-- Only handle concrete addresses for now.
symStep (pc, i, mem, _:_:stack, cs) Store =
    pure (pc+1, i, mem, stack, cs)
symStep _ Store = error "Store expects two arguments."

symStep (pc, i, mem, SCon addr:stack, cs) Load =
    case M.lookup addr mem of
        Just w -> pure (pc+1, i, mem, w:stack, cs)
        Nothing -> error "Nothing to Load at address."
-- Only handle concrete addresses for now.
symStep (pc, i, mem, _:stack, cs) Load =
    pure (pc+1, i+1, mem, SAny i: stack, cs)
symStep _ Load = error "Store expects two arguments."

symStep _ Done = error "No step for Done"

defaultSymState :: SymState
defaultSymState = (0, 0, M.empty, [], [])

instance Show Sym where
    show (SAdd l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
    show (SCon w) = show (wordToSignedInt w)
    show (SAny i) = valName i
    show (SEq l r) = show l ++ " = " ++ show r
    show (SNot c) = "~(" ++ show c ++ ")"
    show (SAnd l r) = show l ++ " and " ++ show r
    show (SOr l r) = show l ++ " or " ++ show r
    show (SLt l r) = show l ++ " < " ++ show r




type SValMap = M.Map Int (S.Symbolic S.SVal)

data SolvedState = SolvedState SymState S.SMTResult

(<>) :: S.Set a -> S.Set a -> S.Set a
(<>) = union

-- Walk the constraint gathering up the free variables.
gatherFree :: Sym -> S.Set Sym
gatherFree c@(SAny _) = S.singleton c
gatherFree (SAdd l r) = gatherFree l <> gatherFree r
gatherFree (SEq l r) = gatherFree l <> gatherFree r
gatherFree (SNot c) = gatherFree c
gatherFree (SOr l r) = gatherFree l <> gatherFree r
gatherFree (SAnd l r) = gatherFree l <> gatherFree r
gatherFree (SLt l r) = gatherFree l <> gatherFree r
gatherFree (SCon _) = mempty

-- Create an existential word of `i` bits with the name `name`.
sWordEx :: Int -> String -> S.Symbolic S.SVal
sWordEx i name =  ask >>= liftIO . S.svMkSymVar (Just S.EX) (S.KBounded False i) (Just (toList name))

-- Create existential SVals for each of SAny's in the input.
createSym :: [Sym] -> S.Symbolic (M.Map Int S.SVal)
createSym cs = do
    pairs <- traverse createSymPair cs
    return $  M.fromList pairs
    where 
        readableName i = valName $ i
        createSymPair (SAny i) = do
            v <- sWordEx 32 (readableName i)
            return (i, v)
        createSymPair _ = error "Non-variable encountered."

-- Convert a list of path constraints to a symbolic value the SMT solver can solve.
-- Each constraint in the list is conjoined with the others.
toSMT :: [Sym] -> S.Symbolic S.SVal
toSMT c = do
    let freeVars = gatherFree (foldr SAnd (SCon 1) c)
    sValMap <- createSym (S.toList freeVars)
    smts <- traverse (symToSMT sValMap) c
    return $ conjoin smts

symToSMT :: M.Map Int S.SVal -> Sym -> S.Symbolic S.SVal
symToSMT m (SEq l r) =
    sValToSWord <$> (S.svEqual <$> symToSMT m l <*> symToSMT m r)
symToSMT m (SAdd l r) =
    S.svPlus <$> symToSMT m l <*> symToSMT m r
symToSMT _ (SCon w) =  return $ wordToSVal w
symToSMT m (SNot c) =
    let c' = symToSMT m c
    in sValToSWord <$> (S.svNot <$> (sValToSBool <$> c'))
symToSMT m (SOr l r) =
    let l' = sValToSBool <$> symToSMT m l
        r' = sValToSBool <$> symToSMT m r
    in sValToSWord <$> (S.svOr <$> l' <*> r')
symToSMT m (SAnd l r) =
    let l' = sValToSBool <$> symToSMT m l
        r' = sValToSBool <$> symToSMT m r
    in sValToSWord <$> (S.svAnd <$> l' <*> r')
symToSMT m (SLt l r) =
    sValToSWord <$> (S.svLessThan <$> symToSMT m l <*> symToSMT m r)
symToSMT m (SAny i) = do
    case M.lookup i m of
        Just val -> return val
        Nothing -> error "Missing symbolic variable."
        

renderSMTResult :: S.SMTResult -> String
renderSMTResult (S.Unsatisfiable _) = "Unsatisfiable"
renderSMTResult s@(S.Satisfiable _ _) =
    let dict = M.mapKeys fromList $ S.getModelDictionary s
    in if M.null dict then "Trivial" else renderDict dict
renderSMTResult _ = "Error"

renderSolvedState :: SolvedState -> String
renderSolvedState (SolvedState (pc,_,_,st,cs) c) =
    "PC: " ++ show pc ++ "\n" ++
    "Stack: " ++ (show <$> st) ++ "\n" ++
    "Path Constraints: " ++ show (foldr SAnd (SCon 1) cs) ++ "\n" ++
    "Solved Values: " ++ renderSMTResult c
                    
renderDict :: (Show v) => M.Map String v -> String
renderDict m =
    foldr toStr "" (M.toList m)
    where toStr (k,v) s = k ++ " = " ++ show v ++ ", " ++ s

solveSym :: Trace -> IO (T.Tree SolvedState)
solveSym (T.Node state@(_, _, _, _, cs) c) = do
    let smtExpr = toSMT cs
    S.SatResult smtRes <- S.satWith S.z3 smtExpr
    children <- traverse solveSym c
    return $ T.Node (SolvedState state smtRes) children

conjoin :: [S.SVal] -> S.SVal
conjoin = foldr (S.svAnd . sValToSBool) S.svTrue