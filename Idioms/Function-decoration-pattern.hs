-- Function decoration pattern
-- ref: https://wiki.haskell.org/Function_decoration_pattern
-- paper: http://web.cecs.pdx.edu/~ntc2/haskell-decorator-paper.pdf

-- You want to add extra properties to a function type, but you don't want the users to have to tediously project out the decorated type when they don't care about the decorations.
-- This can be generalized to arbitrary values instead of just functions.
-- decorators in Python
-- AOP

{-# LANGUAGE MultiParamTypeClasses, Rank2Types, FlexibleContexts, FlexibleInstances #-}
 
-- This implementation is somewhat general, but it is not intended
-- that all examples can be cast in exactly this way.
data Decorate d a b = Decorated (a -> b) (d a b)
 
class Decorated d a b dec where
    decorated :: (a -> b) -> d a b -> dec a b 
    -- The above is a Scott-encoding of the below which is equivalent.
    -- The Scott-encoded version is often more convenient and efficient.`
    -- decorated :: Decorate d a b -> dec a b
 
instance Decorated d a b (Decorate d) where
    decorated = Decorated
 
instance Decorated d a b (->) where
    decorated f _ = f
 
type IsDecorated d a b = forall dec. Decorated d a b dec => dec a b 
 
-- Not a very realistic example.
type UnitTested = Decorate (,)
type IsUnitTested a b = IsDecorated (,) a b
 
makeTested :: (a -> b) -> a -> b -> IsUnitTested a b
makeTested f a b = decorated f (a, b)
 
test :: Eq b => UnitTested a b -> Bool
test (Decorated f (a, b)) = f a == b
 
testedSquare :: Num a => IsUnitTested a a
testedSquare = makeTested (\x -> x * x) 3 9
 
main = do
    print (map testedSquare [1,2,3])
    putStrLn (if test testedSquare then "Passed" else "Failed")

-- An isomorphism is a function equipped with an inverse. Traditionally, this would be represented by a data type such as

class Isomorphic a b iso where
    iso :: (a -> b) -> (b -> a) -> iso a b
 
instance Isomorphic a b Iso where
    iso = Iso
 
instance Isomorphic a b (->) where
    iso to _ = to
 
type IsIsomorphic a b = forall iso. Isomorphic a b iso => iso a b

-- This is closely related to the Yoneda lemma and representability. Essentially we are identifying the value x with ($ x). The instances of the type classes just choose how we want to observe the x via ($ x) observation. makeTested makes this pretty explicit especially if the direct (rather than Scott-encoded) representation is used.

-- This can be viewed as a special case of the finally, tagless encoding.

-- let in do == let xxx = yyy in
trace levelRef name f = curry traced where
    traced args = do
        level <- readIORef levelRef
        let prefix = concat . replicate level $ "| "
        putStrLn $ prefix ++ name ++ show args
        modifyIORef levelRef (+1)
        r <- uncurryM f args
        modifyIORef levelRef (subtract 1)
        putStrLn $ prefix ++ show r
        return r

memoize cacheRef f = curry memoized where
    memoized args = do
        cache <- readIORef cacheRef
        case Map.lookup args cache of
            Just r -> return r
            Nothing -> do
                r <- uncurryM f args
                modifyIORef cacheRef (Map.insert args r)
                return r
