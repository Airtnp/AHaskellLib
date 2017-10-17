{-# LANGUAGE CPP #-}
module Y where

newtype Rec a = Rec { recOut :: Rec a -> a }

y :: (a -> a) -> a
y = \f ->
        (\x -> 
            f (recOut x $ x)
        )
        (
            Rec (
                \x ->
                    f (recOut x $ x)
            )
        )

#ifndef __GLASGOW_HASKELL__

fix f = y f

#else

fix f = fixH (Rec fixH)
    where
        {-# NOINLINE fixH #-}
        fixH x = f (recOut x $ x)

#endif

newtype RecM a = RecM (RecM a -> a)
y2 = \f ->
        (\h -> 
            h $ RecM h
        )
        (
            \x ->
                f . (\(RecM g) ->
                    g    
                ) x $ x
        )