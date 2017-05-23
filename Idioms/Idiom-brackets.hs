-- Idiom brackets
-- ref: https://wiki.haskell.org/Idiom_brackets

-- f <$> x <*> y
-- Since f is not a pure function, it's f :: a -> b -> m c. The correct form would be
-- join $ f <$> x <*> y

-- f a b = [a+b]
-- join $ f <$> [1] <*> [2] = [3]

-- Idiom brakcets
-- Type class hackery to eliminate the 'join':

class Applicative i => Idiomatic i f g | g -> f i where
    idiomatic :: i f -> g
 
iI :: Idiomatic i f g => f -> g
iI = idiomatic . pure
 
data Ii  =  Ii
 
instance Applicative i    => Idiomatic i x (Ii -> i x) where
    idiomatic xi Ii = xi
 
instance Idiomatic i f g  => Idiomatic i (s -> f) (i s -> g) where
    idiomatic sfi si = idiomatic (sfi <*> si)

-- iI f x y Ii = f <$> x <*> y

data Ji = Ji
 
instance (Monad i, Applicative i)    => Idiomatic i (i x) (Ji -> i x) where
    idiomatic xii Ji = join xii

-- iI f x y Ji = join $ f <$> x <*> y

data J   = J
 
instance (Monad i, Idiomatic i f g) => Idiomatic i (i f) (J -> g) where
    idiomatic fii J = idiomatic (join fii)

-- so you can insert joins wherever you like, thus:

-- iI f x y J z Ii 
-- = join (f <$> x <*> y) <*> z
-- = do {x' <- x; y' <- y; f' <- f x y; z' <- z; return (f' z')}

