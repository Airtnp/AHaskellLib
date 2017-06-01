-- Unsafe functions
-- ref: https://wiki.haskell.org/Unsafe_functions

unsafePerformIO :: IO a -> a
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveST :: ST s a -> ST s a
unsafeIOToST :: IO a -> ST s a
unsafeIOToSTM :: IO a -> STM a
unsafeFreeze, unsafeThaw
unsafeCoerce# :: a -> b
seq :: a -> b -> b

-- Unsafe functions can break type safety (unsafeCoerce#, unsafePerformIO), interfere with lazy IO (unsafeInterleaveIO), or break parametricity (seq). Their use (except in the case of seq) would require some kind of assurance on the part of the programmer that what they're doing is safe. 

-- "unsafe" is also a keyword which can be used in a foreign import declaration. (FFI)




