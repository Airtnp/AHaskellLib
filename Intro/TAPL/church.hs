import Unsafe.Coerce


churchTrue = \t -> \f -> t
churchFalse = \t -> \f -> f

churchAnd = \a -> \b -> a b churchFalse

churchPair = \f -> \s -> \b -> b f s
churchFirst = \p -> p churchTrue
churchSecond = \p -> p churchFalse

churchZero = \s -> \z -> z
churchSucc = \n -> \s -> \z -> s (n s z)
churchOne = churchSucc churchZero

churchPlus = \m -> \n -> \s -> \z -> m s (n s z)        -- \m -> \f -> \n -> \z ==> fix (churchPlus churchZero) $ churchOne == churchOne
churchMul = \m -> \n -> m (churchPlus n) churchZero
churchIsZero = \m -> m (\x -> churchFalse) churchTrue
churchExp = \m -> \n -> \s -> \z -> (\s' -> m n s') s z

churchPred = \m -> churchFirst (m ss zz) where
    zz = churchPair churchZero churchZero
    ss = \p -> churchPair (churchSecond p) (churchPlus churchOne $ churchSecond p)

churchMinus = \m -> \n -> (n churchPred) m
churchLeq = \m -> \n -> churchIsZero (churchMinus m n)
-- cannot construct infinite type
-- segmentation fault
churchEq = \m -> \n -> 
    churchAnd 
        (churchLeq (unsafeCoerce m) n) 
        (churchLeq n (unsafeCoerce m))

realbool = \b -> b True False
realnat = \m -> m (+1) 0

-- cannot construct infinite type
-- use unsafeCoerce :: a -> b
y :: (a -> a) -> a
y = \f ->
        (\x -> f (
            unsafeCoerce x x)) -- \y -> unsafeCoerce x x y
        (\x -> f (
            unsafeCoerce x x))
