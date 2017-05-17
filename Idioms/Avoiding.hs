-- Avoiding : Avoiding IO/Avoding partial function
-- ref: https://wiki.haskell.org/Avoiding_IO
-- ref: https://wiki.haskell.org/Avoiding_partial_functions

-- Avoiding IO

    -- Lazy construction
    
        --  avoid a series of output functions by constructing a complex data structure with non-IO code and output it with one output function.

        -- replicateM_ 10 (putStr "foo")

        -- replaced by

        -- putStr (concat $ replicate 10 "foo"")

        -- do
            -- h <- openFile "foo" WriteMode
            -- replicateM_ 10 (hPutStr h "bar")
            -- hClose h

        -- replaced by

        -- writeFile "foo" (concat $ replicate 10 "bar")

    -- Writer Monad

        -- IO -> Writer

    -- State Monad

        -- IORef -> State

        -- A function which computes a random value with respect to a custom distribution (distInv is the inverse of the distribution function)

        -- randomDist :: (Random a, Num a) => (a -> a) -> IO a
        -- randomDist distInv = liftM distInv (randomRIO (0,1))

        -- replaced by

        -- randomDist :: (RandomGen g, Random a, Num a) => (a -> a) -> State g a
        -- randomDist distInv = liftM distInv (State (randomR (0,1)))

        -- evalState (randomDist distInv) (mkStdGen an_arbitrary_seed)

    -- ST Monad

        -- For this kind of application the State Thread monad ST was invented. It provides IORef -> STRef, IOArray -> STArray, IOUArray -> STUArray, and you can define new operations in ST, but then you need to resort to unsafe operations by using the unsafeIOtoST function.

    -- Applicative functor style

        -- You can only call this function within the IO monad, and it is not very efficient either, since for every translation the dictionary must be read from disk. You can rewrite this function in a way that it generates a non-monadic function that can be used anywhere.

        -- translate :: String -> IO String
        -- translate word =
            -- do dict <- readDictionary "english-german.dict"
            -- return (Map.findWithDefault word word dict)

        -- replaced by

        -- makeTranslator :: IO (String -> String)
        -- makeTranslator =
        -- do dict <- readDictionary "english-german.dict"
        --     return (\word -> Map.findWithDefault word word dict)
        
        -- main :: IO ()
        -- main =
        -- do translate <- makeTranslator
        --     putStr (unlines (map translate ["foo", "bar"]))

    -- Custom monad type class

        -- If you only use a small set of IO operations in otherwise non-IO code you may define a custom monad type class which implements just these functions. You can then implement these functions based on IO for the application and without IO for the test suite.

        -- A function which converts an English phrase to the currently configured user language of the system.
        -- localeTextIO :: String -> IO String

        -- replaced by

        -- class Monad m => Locale m where
        -- localeText :: String -> m String
        
        -- instance Locale IO where
        -- localeText = localeTextIO
        
        -- instance Locale Identity where
        -- localeText = Identity

    -- unsafePerformIO

        -- When you apply it, think about how to reduce its use and how you can encapsulate it in a library with a well chosen interface. Since unsafePerformIO makes functions look like non-IO functions, they should also behave like non-IO functions. 
        
        -- file access must not be hidden in unsafePerformIO, whereas careful memory manipulation may be safe – see for instance the Data.ByteString module.

        -- global mutable variable
        -- {-# NOINLINE bla #-}
        -- var :: IORef Int / MVar / TVar
        -- var = unsafePerformIO (newIORef 10)


-- Avoiding partial functions

    -- For a partial function f the general pattern is: Wherever we write "check whether x is in the domain of f before computing f x", we replace it by combination of check and computation of f.

    -- total function vs. partial function


