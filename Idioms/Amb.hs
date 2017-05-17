-- Amb : amb operator
-- ref: https://wiki.haskell.org/Amb
-- ref: http://www.randomhacks.net.s3-website-us-east-1.amazonaws.com/2005/10/11/amb-operator/

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Identity

{-
    # A list of places we can "rewind" to
    # if we encounter amb with no
    # arguments.
    $backtrack_points = []

    # Rewind to our most recent backtrack
    # point.
    def backtrack
    if $backtrack_points.empty?
        raise "Can't backtrack"
    else
        $backtrack_points.pop.call
    end
    end

    # Recursive implementation of the
    # amb operator.
    def amb *choices
    # Fail if we have no arguments.
    backtrack if choices.empty?
    callcc {|cc|
        # cc contains the "current
        # continuation".  When called,
        # it will make the program
        # rewind to the end of this block.
        $backtrack_points.push cc

        # Return our first argument.
        return choices[0]
    }

    # We only get here if we backtrack
    # using the stored value of cc,
    # above.  We call amb recursively
    # with the arguments we didn't use.
    amb *choices[1...choices.length]
    end

    # Backtracking beyond a call to cut
    # is strictly forbidden.
    def cut
    $backtrack_points = []
    end
-}


{-
    # amb will (appear to) choose values
    # for x and y that prevent future
    # trouble.
    x = amb 1, 2, 3
    y = amb 4, 5, 6

    # Ooops! If x*y isn't 8, amb would
    # get angry.  You wouldn't like
    # amb when it's angry.
    amb if x*y != 8

    # Sure enough, x is 2 and y is 4.
    puts x, y 
-}

newtype AmbT r m a = AmbT { unAmbT :: StateT [AmbT r m r] (ContT r m) a }
type Amb r = AmbT r Identity
 
instance MonadTrans (AmbT r) where
    lift = AmbT . lift . lift
 
instance (Monad m) => Monad (AmbT r m) where
    AmbT a >>= b = AmbT $ a >>= unAmbT . b
    return = AmbT . return
 
backtrack :: (Monad m) => AmbT r m a
backtrack = do xss <- AmbT get
               case xss of
                 [] -> fail "amb tree exhausted"
                 (f:xs) -> do AmbT $ put xs; f; return undefined
 
addPoint :: (Monad m) => (() -> AmbT r m r) -> AmbT r m ()
addPoint x = AmbT $ modify (x () :)
 
amb :: (Monad m) => [a] -> AmbT r m a
amb []     = backtrack
amb (x:xs) = ambCC $ \exit -> do
               ambCC $ \k -> addPoint k >> exit x
               amb xs
    where ambCC f = AmbT $ callCC $ \k -> unAmbT $ f $ AmbT . k
 
cut :: (Monad m) => AmbT r m ()
cut = AmbT $ put []
 
runAmbT :: (Monad m) => AmbT r m r -> m r
runAmbT (AmbT a) = runContT (evalStateT a []) return
 
runAmb :: Amb r r -> r
runAmb = runIdentity . runAmbT

example :: Amb r (Integer,Integer)
example = do x <- amb [1,2,3]
             y <- amb [4,5,6]
             if x*y == 8
               then return (x,y)
               else amb []