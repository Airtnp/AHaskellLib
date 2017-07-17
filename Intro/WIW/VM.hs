{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

type Stack = [Int]
type Output = [Int]
type Program = [Instr]
type VM a = ReaderT Program (WriterT Output (State Stack)) a
newtype Comp a = Comp { unComp :: VM a }
    deriving (Monad, MonadReader Program, MonadWriter Output, MonadState Stack)
data Instr = Push Int | Pop | Puts

evalInstr :: Instr -> Comp ()
evalInstr instr = case instr of
    Pop -> modify tail
    Push n -> modify (n:)
    Puts -> do
        tos <- gets head
        tell [tos]

eval :: Comp ()
eval = do
    instr <- ask
    case instr of
        [] -> return ()
        (i:is) -> evalInstr i >> local (const is) eval

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval)

program :: Program
program = [
        Push 42,
        Push 27,
        Puts,
        Pop,
        Puts,
        Pop
    ]
    
main :: IO ()
main = mapM_ print $ execVM program

-- Another hoist implementation

import Control.Monad.State
import Control.Monad.Morph

type Eval a = State [Int] a
runEval :: [Int] -> Eval a -> a
runEval = flip evalState

pop :: Eval Int
pop = do
    top <- gets head
    modify tail
    return top

push :: Int -> Eval ()
push x = modify (x:)

ev1 :: Eval Int
ev1 = do
    push 3
    push 4
    pop
    pop

-- hoist :: Monad m => (forall a. m a -> n a) -> t m b -> t n b
-- generalize :: Monad m => Identity a -> m a
ev2 :: StateT [Int] IO ()
ev2 = do
    result <- hoist generalize ev1
    liftIO $ putStrLn $ "Result: " ++ show result