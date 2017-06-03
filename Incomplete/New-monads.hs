-- New monads
-- ref: https://wiki.haskell.org/New_monads

-- MonadBase

-- MonadLib

-- MonadRandom

-- MaybeT

-- MonadSupply

-- MonadUndo

-- MonadUnique

-- MonadSTO

-- MonadNondet

-- Stateful nondeterminism

    -- If you want to do nondeterministic computation with local states for each of your threads and a global state shared by all your threads, use this monad:

 
instance Monad (SuperState s t) where 
    return a        = SuperState $ \s t -> (t,[(a,s)])
    (SuperState x) >>= f = SuperState $ \s t -> let (t',stateList) = x s t
                                                in  foldl (\(newt,sofar) (v,s) -> let (t'',lst) = runSuperState (f v) s newt
                                                                               in
                                                                                 (t'',sofar++lst)) (t',[]) stateList
 
 
 
instance MonadPlus (SuperState s t) where
    mzero = mz
    mplus = mp
 
 
getGlobal = SuperState $ \s t-> (t,[(t,s)])
getLocal = SuperState $ \s t -> (t,[(s,s)])
 
putLocal s = SuperState $ \_ t -> (t,[((),s)])
putGlobal t = SuperState $ \s _ -> (t,[((),s)])
 
mz = SuperState $ \_ t -> (t,[])
mp (SuperState a) (SuperState b) = 
    SuperState $ \s t ->
        let
            (t',stateList) = a s t
            (t'',stateList') = b s t'
        in
          (t'',stateList++stateList')


-- MonadAdvSTM

-- MonadExit

-- MonadSplit

-- Lazy/Strict variants
    -- LazyWriterT
    -- Strict RWS