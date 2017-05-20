-- Error vs. Exception
-- ref: https://wiki.haskell.org/Error_vs._Exception
-- ref: https://wiki.haskell.org/Error
-- ref: https://wiki.haskell.org/Exception

-- Error

{-
    An error denotes a programming error.

    The Prelude function error represents an error with a message, undefined represents an error with a standard message. For a Haskell program, an undefined value cannot be distinguished from an infinite loop, e.g. let x=x+1 in x :: Int. Almost not.
    
    So error and undefined value are somehow synonyms in Haskell.

    Since Haskell is non-strict, the occurence of an error within an expression is not necessarily an error, if the erroneous value is ignored somewhere, e.g.  False && undefined.
    
    However, if an expression finally evaluates to bottom or hangs in an infinite loop, then this is definitely a programming error.
-}

-- Exception

{-
    An exception denotes an unpredictable situation at runtime, like "out of disk storage", "read protected file", "user removed disk while reading", "syntax error in user input". These are situation which occur relatively seldom and thus their immediate handling would clutter the code which should describe the regular processing. Since exceptions must be expected at runtime there are also mechanisms for (selectively) handling them.

    (Control.Exception.try, Control.Exception.catch) Unfortunately Haskell's standard library names common exceptions of IO actions IOError and the module Control.Monad.Error is about exception handling not error handling.
    
    In general you should be very careful not to mix up exceptions with errors. Actually, an unhandled exception is an error.
-}

    -- Implementation
        -- Exception Monad
        -- error_code vs. exception in C++
        -- Functions return error codes, but the handling of error codes does not uglify the calling code. in Haskell

-- First we implement exception handling for non-monadic functions. Since no IO functions are involved, we still cannot handle exceptional situations induced from outside the world, but we can handle situations where it is unacceptable for the caller to check a priori whether the call can succeed.

data Exceptional e a =
     Success a
   | Exception e
   deriving (Show)
 
instance Monad (Exceptional e) where
   return              =  Success
   Exception l >>= _   =  Exception l
   Success  r  >>= k   =  k r
 
throw :: e -> Exceptional e a
throw = Exception
 
catch :: Exceptional e a -> (e -> Exceptional e a) -> Exceptional e a
catch (Exception  l) h = h l
catch (Success r)    _ = Success r    

newtype ExceptionalT e m a =
   ExceptionalT {runExceptionalT :: m (Exceptional e a)}
 
instance Monad m => Monad (ExceptionalT e m) where
   return   =  ExceptionalT . return . Success
   m >>= k  =  ExceptionalT $
      runExceptionalT m >>= \ a ->
         case a of
            Exception e -> return (Exception e)
            Success   r -> runExceptionalT (k r)
 
throwT :: Monad m => e -> ExceptionalT e m a
throwT = ExceptionalT . return . Exception
 
catchT :: Monad m =>
   ExceptionalT e m a -> (e -> ExceptionalT e m a) -> ExceptionalT e m a
catchT m h = ExceptionalT $
   runExceptionalT m >>= \ a ->
      case a of
         Exception l -> runExceptionalT (h l)
         Success   r -> return (Success r)
 
bracketT :: Monad m =>
   ExceptionalT e m h ->
   (h -> ExceptionalT e m ()) ->
   (h -> ExceptionalT e m a) ->
   ExceptionalT e m a
bracketT open close body =
   open >>= (\ h ->
      ExceptionalT $
         do a <- runExceptionalT (body h)
            runExceptionalT (close h)
            return a)

data IOException =
     DiskFull
   | FileDoesNotExist
   | ReadProtected
   | WriteProtected
   | NoSpaceOnDevice
   deriving (Show, Eq, Enum)
 
open :: FilePath -> ExceptionalT IOException IO Handle
 
close :: Handle -> ExceptionalT IOException IO ()
 
read :: Handle -> ExceptionalT IOException IO String
 
write :: Handle -> String -> ExceptionalT IOException IO ()
 
readText :: FilePath -> ExceptionalT IOException IO String
readText fileName =
   bracketT (open fileName) close $ \h ->
      read h

-- Finally we can escape from the Exception monad if we handle the exceptions completely.

    {-
        main :: IO ()
        main =
        do result <- runExceptionalT (readText "test")
            case result of
                Exception e -> putStrLn ("When reading file 'test' we encountered exception " ++ show e)
                Success x -> putStrLn ("Content of the file 'test'\n" ++ x)
    -}

    -- Processing individual exceptions

        {-
            So far I used the sum type IOException that subsumes a bunch of exceptions. However, not all of these exceptions can be thrown by all of the IO functions. E.g. a read function cannot throw WriteProtected or NoSpaceOnDevice. Thus when handling exceptions we do not want to handle WriteProtected if we know that it cannot occur in the real world.
            
            We like to express this in the type and actually we can express this in the type.

            Consider two exceptions: ReadException and WriteException. In order to be able to freely combine these exceptions, we use type classes, since type constraints of two function calls are automatically merged.
        -}

import Control.Monad.Exception.Synchronous (ExceptionalT, )
 
class ThrowsRead  e where throwRead  :: e
class ThrowsWrite e where throwWrite :: e
 
readFile  :: ThrowsRead  e => FilePath -> ExceptionalT e IO String
writeFile :: ThrowsWrite e => FilePath -> String -> ExceptionalT e IO ()

        {-
            copyFile src dst =
                writeFile dst =<< readFile src
            copyFile ::
                (ThrowsWrite e, ThrowsRead e) =>
                FilePath -> FilePath -> ExceptionalT e IO ()
        -}        

data ApplicationException =
      ReadException
    | WriteException
 
instance ThrowsRead ApplicationException where
    throwRead = ReadException
 
instance ThrowsWrite ApplicationException where
    throwWrite = WriteException

-- Defining exception types as a sum of "this particular exception" and "another exception" lets us compose concrete types that can carry a certain set of exceptions on the fly. This is very similar to switching from particular monads to monad transformers. Thanks to the type class approach the order of composition needs not to be fixed by the throwing function but is determined by the order of catching. We even do not have to fix the nested exception type fully when catching an exception. It is enough to fix the part that is interesting for catch:
data ReadException e =
      ReadException
    | NoReadException e
 
instance ThrowsRead (ReadException e) where
     throwRead = ReadException
 
instance ThrowsWrite e => ThrowsWrite (ReadException e) where
     throwWrite = NoReadException throwWrite
 
 
data WriteException e =
      WriteException
    | NoWriteException e
 
instance ThrowsRead e => ThrowsRead (WriteException e) where
     throwRead = NoWriteException throwRead
 
instance ThrowsWrite (WriteException e) where
     throwWrite = WriteException

import Control.Monad.Exception.Synchronous (Exceptional(Success,Exception))
 
catchRead :: ReadException e -> Exceptional e String
catchRead ReadException = Success "catched a read exception"
catchRead (NoReadException e) = Exception e
 
throwReadWrite :: (ThrowsRead e, ThrowsWrite e) => e
throwReadWrite =
    asTypeOf throwRead throwWrite
 
exampleCatchRead :: (ThrowsWrite e) => Exceptional e String
exampleCatchRead =
    catchRead throwReadWrite

-- Note how in exampleCatchRead the constraint ThrowsRead is removed from the constraint list of throwReadWrite.




-- the term exception for expected but irregular situations at runtime -- the term error for mistakes in the running program that can be resolved only by fixing the program.
    -- Haskell's error is just sugar for undefined

-- ways of exceptions
    -- Maybe/Either/IO exceptions

-- Exceptions: Prelude.catch, Control.Exception.catch, Control.Exception.try, IOError, Control.Monad.Error
-- Errors: error, assert, Control.Exception.catch, Debug.Trace.trace

{-
    My conclusion is that (programming) errors can only be handled by the programmer, not by the running program. Thus the term "error handling" sounds contradictory to me. However supporting a programmer with finding errors (bugs) in their programs is a good thing. I just wouldn't call it "error handling" but "debugging".

    An important example in Haskell is the module Debug.Trace. It provides the function trace that looks like a non-I/O function
    but actually outputs something on the console. It is natural that debugging functions employ hacks. For finding a programming error it would be inappropriate to transform the program code to allow I/O in a set of functions that do not need it otherwise. The change would only persist until the bug is detected and fixed. Summarized, hacks in debugging functions are necessary for quickly finding problems without large restructuring of the program and they are not problematic, because they only exist until the bug is removed.

    Different from that exceptions are things you cannot fix in advance. You will always have to live with files that cannot be found and user input that is malformed. You can insist that the user does not hit the X key, but your program has to be prepared to receive a "X key pressed" message nonetheless. Thus exceptions belong to the program and the program must be adapted to treat exceptional values where they can occur. No hacks can be accepted for exception handling.
-}

-- When exceptions -> errors

    -- It is an error to not handle an exception. If a file cannot be opened you must respect that result. You can proceed as if the file could be opened, though. If you do so you might crash the machine or the runtime system terminates your program. All of these effects are possible consequences of a (programming) error.

    -- Again, it does not matter whether the exceptional situation is signaled by a return code that you ignore or an IO exception for which you did not run a catch.

-- When errors -> exceptions

    -- Typical examples are: A process in an operating system shall not crash the whole system if it crashes itself. A buggy browser plugin shall not terminate the browser. A corrupt CGI script shall not bring the web server down, where it runs on.

-- Errors and Type system

    -- It is generally considered, that errors in a program imply a lack in the type system. If the type system would be strong enough and the programmers would be patient enough to work out the proofs imposed by library functions, then there would be no errors in programs at all, only exceptions.

    -- An alternative to extending the type system to dependent type system that allows for a wide range of proofs is the Extended Static Checking. For example:

{-# CONTRACT head :: { xs | not (null xs) } -> Ok #-}
head :: [a] -> a
head []    = error "head: empty list"
head (x:_) = x

-- Call stacks

-- Escaping from control structures

    -- break/return... in imperative languages

    -- what exception handlers and resource deallocators shall be run when you leave a loop or function using break? Analogously exceptions can also be used to escape from custom control structures (yeah, higher order functions are also possible in imperative languages) or deep recursive searches. In imperative languages exceptions are often implemented in a way that is especially efficient when deep recursions have to be aborted.

    -- Escaping from a control structure is just the irregular case with respect to the regular case of looping/descending in recursion. In Haskell, when you use exception monads like Control.Monad.Exception.Synchronous or Control.Monad.Error, exceptions are just an automated handling of return codes.