module Calculator where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    -- fmap :: (a -> b) -> State s a -> State s b
    -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
    fmap f fs = State $ \s ->
        let (a, s') = runState fs s
        in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    f <*> fa = State $ \s ->
        let (fab, s0) = runState f s
            (a, s1) = runState fa s0
        in (fab a, s1)

instance Monad (State s) where
    return = pure
    -- (>>=) :: State s a -> (a -> State s b) -> State s b
    fa >>= f = State $ \s ->
        let (a, s') = runState fa s
        in runState (f a) s'

get :: State s s
get = State $ \s -> (s, s)

gets :: (s -> a) -> State s a
gets f =  State $ \s -> (f s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

(~+) :: Double -> State Double (Double -> Double)
(~+) x = State $ \s -> ((+x), s + x)

(~-) :: Double -> State Double (Double -> Double)
(~-) x = State $ \s -> (((-)x), s - x)

(~*) :: Double -> State Double (Double -> Double)
(~*) x = State $ \s -> ((*x), s * x)

(~/) :: Double -> State Double (Double -> Double)
(~/) x = State $ \s -> ((/x), s / x)

-- repeat last operation
(~~) :: (Double -> Double) -> State Double (Double -> Double)
(~~) f = State $ \s -> (f, f s)

op :: State Double (Double -> Double)
op = do
    (~+) 10
    (~*) 4
    (~-) 2
    (~/) 10
    >>= (~~)
    >>= (~~)

main :: IO ()
main = do
    let (_, result) = runState op 0
    print result
    