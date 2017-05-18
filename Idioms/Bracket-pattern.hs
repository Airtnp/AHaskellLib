-- Bracket pattern
-- ref: https://wiki.haskell.org/Bracket_pattern

{-
    When acquiring, using, and releasing various resources, it can be quite convenient to write a function to manage the acquisition and releasing, taking a function of the acquired value that specifies an action to be performed in between.

    The function bracket from Control.Exception can be used to build such functions in the IO monad. It is commonly partially applied to its first two parameters, giving a function which manages the resource in question, allocating it for the context of a specific action.
-}

-- Control.Exception

-- | Like 'finally', but only performs the final action if there was an
-- exception raised by the computation.
onException :: IO a -> IO b -> IO a
onException io what = io `catch` \e -> do _ <- what
                                          throwIO (e :: SomeException)

bracket :: IO a        -- computation to run first ("acquire resource")
        -> (a -> IO b) -- computation to run last ("release resource")
        -> (a -> IO c) -- computation to run in-between
        -> IO c
bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

finally :: IO a         -- ^ computation to run first
        -> IO b         -- ^ computation to run afterward (even if an exception was raised)
        -> IO a         -- returns the value from the first computation
a `finally` sequel =
  mask $ \restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r

-- In general, throughout the libraries many functions having names     beginning with with are defined to manage various resources in this fashion. One passes in a function of the allocated resource that says what to do, and the with-function handles both the allocation and deallocation. For example, in System.IO, there is a function:
    -- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
-- that manages opening and closing a filehandle, or in Foreign.Marshal.Array, there is a function
    -- withArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b

-- | When you want to acquire a resource, do some work with it, and
-- then release the resource, it is a good idea to use 'bracket',
-- because 'bracket' will install the necessary exception handler to
-- release the resource in the event that an exception is raised
-- during the computation.  If an exception is raised, then 'bracket' will
-- re-raise the exception (after performing the release).
--
-- A common example is opening a file:
--
-- > bracket
-- >   (openFile "filename" ReadMode)
-- >   (hClose)
-- >   (\fileHandle -> do { ... })
--
-- The arguments to 'bracket' are in this order so that we can partially apply
-- it, e.g.:
--
-- > withFile name mode = bracket (openFile name mode) hClose
--


-- For this one can use what is effectively sequence in the Cont monad, "unwrapped" appropriately:
nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (sequence (map Cont xs))

withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 strings act = nest (map withCString strings)
                                     (\rs -> withArray0 nullPtr rs act)