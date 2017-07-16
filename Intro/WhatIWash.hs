-- What I Wish I Knew When Learning Haskell

-- non-exhaustive
-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Debug.Trace
-- trace/traceShow/tracePrintf
-- trace :: String -> a -> a

-- Type holes

-- Deferred Type Errors
-- check type at runtime
-- {-# OPTIONS_GHC -fdefer-type-errors #-}

-- Reader
-- The reader monad lets us access shared immutable state within a monadic context.
computation :: Reader MyContext (Maybe String)
computation = do
    n <- asks bar
    x <- asks foo
    if n > 0
        then return (Just x)
        else return Nothing

newtype Reader r a = Reader { runReader :: r -> a }
instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ runReader m . f

-- Wrtier
-- The writer monad lets us emit a lazy stream of values from within a monadic

example :: Writer [Int] String
example = do
    tell [1..5]
    tell [5..10]
    return "foo"

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Monoid w => Monad (Writer w) where
    return a = Writer (a, mempty)
    m >>= k = Writer $ let
        (a, w) = runWriter m
        (b, w') = runWriter (k a)
        in (b, w `mappend` w')

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

tell :: w -> Writer w ()
tell w = Writer ((), w)

-- This implementation is lazy so some care must be taken that one actually wants to only generate a stream of thunks. Most often the lazy writer is not suitable for use, instead implement the equivalent structure by embedding some monomial object inside a StateT monad, or using the strict version.

-- State Monad
-- The state monad allows functions within a stateful monadic context to access and modify shared state.

test :: State Int Int
test = do
    put 3
    modify (+1)
    get

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    State act >>= k = State $ \s ->
        let (a, s') = act s
        in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

-- Monad Transformer (mtl)
type State s = StateT s Identity
type Writer w = WriterT w Identity
type Reader r = ReaderT r Identity
instance Monad m => MonadState s (StateT s m)
instance Monad m => MonadReader r (ReaderT r m)
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m)

class MonadTrans t where
    lift :: Monad m => m a -> t m a
    -- lift . return === return
        -- lift (return x) = return x
    -- lift m >>= \x -> lift (f x) === lift $ m >>= \x -> f x
        -- lift (m >>= f) = lift m >>= (lift . f)
    
-- Monad (m :: * -> *)
-- MonadTrans (t :: (* -> *) -> * -> *)
    