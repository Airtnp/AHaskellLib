-- Conduit
-- ref: https://wiki.haskell.org/Conduit
-- conduit-lib: https://www.schoolofhaskell.com/user/snoyberg/library-documentation/conduit-overview

{-
    Streaming data library
    
    Collection of libraries that share the same underlying data structures
    
    Alternative to lazy I/O
    
    Promises: deterministic resource management (memory, file descriptors)
-}

-- example
import Conduit
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Binary as CB
import Data.Word8 (toUpper)

main :: IO ()
main = runResourceT
     $ CB.sourceFile "input.txt"
    $= omapCE toUpper
    $= takeCE 500
    $$ CB.sinkFile "output.txt"

{-
    Core datatype: data ConduitM i o m r
    
    Each conduit has:
        Upstream (i): stream of incoming values
        Downstream (o): stream of outgoing values
        Monad (m): conduits are monad transformers
        Result value (r): just like all monads

        Producer ignores its upstream
        Consumer ignores its downstream
        Source has no upstream
        Sink has no downstream
        Conduit has both upstream and downstream

        Producer unifies to Source and Conduit
        Consumer unifies to Conduit and Sink
-}

-- vs. Pipes
-- ref: https://twanvl.nl/blog/haskell/conduits-vs-pipes