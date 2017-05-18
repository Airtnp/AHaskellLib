-- blow your mind
-- ref: https://wiki.haskell.org/Blow_your_mind


-- 1. List/String operation

-- unfoldr :: (b -> Maybe(a, b)) -> b -> [a]

-- words

-- const 强行转 Maybe 搞 unfoldr...
words1 :: [Char] -> [[Char]]
words1 = unfoldr (\b -> 
    fmap (const . (second $ drop 1) . break (==' ') $ b) . listToMaybe $ b)

words2 :: [Char] -> [[Char]]
words2 = takeWhile (not . null) . evalState (repeatM $ modify (drop 1)
         >> State (break (==' '))) . (' ' :)
         where repeatM = sequence . repeat

words3 :: [Char] -> [[Char]]
words3 = fix (\f l -> if null l then [] else let (s, e) = break (==' ') l in s:f (drop 1 e))


-- split in two (alternating)
-- '1234567' -> ('1357', '246')

-- the lazy match with ~ is necessary for efficiency, especially enabling 
-- processing of infinite lists
split1 :: [Char] -> ([Char], [Char])
split1 = foldr (\a ~(x,y) -> (a:y,x)) ([],[])

-- (***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c') infixr 3
-- Split the input between the two argument arrows and combine their output. Note that this is in general not a functor.
-- The default definition may be overridden with a more efficient version if desired.
split2 = (map snd *** map snd) . partition (even . fst) . zip [0..]

split3 = transpose . unfoldr (\a -> toMaybe (null a) (splitAt 2 a))

toMaybe b x = if b then Just x else Nothing
-- or generalize it:
-- toMaybe = (toMonadPlus :: Bool -> a -> Maybe a)
toMonadPlus b x = guard b >> return x

-- MonadPlus: Monads that also support choice and failure.

-- splitting into lists of length N
-- "1234567" -> ["12", "34", "56", "7"]
splitN1 :: Int -> String -> [String]
splitN1 n = (\a -> toMaybe (not $ null a) (splitAt n a))

splitN2 n = takeWhile (not . null) . unfoldr (Just . splitAt n)

ensure :: MonadPlus m => (a -> Bool) -> a -> m a
ensure p x = guard (p x) >> return x

splitN3 n = unfoldr (ensure (not . null . fst) . splitAt n)

-- sortBy
-- comparing :: Ord a => (b -> a) -> b -> b -> Ordering = compare `on` f
-- compare :: Ord a => a -> a -> Ordering
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-- the so called "Schwartzian Transform" for computationally more expensive functions.
sortBy1 f = sortBy (comparing f)
sortBy2 = map snd . sortBy (comparing fst) . map (length &&& id)

-- [item] -> [item, value] -> sorted [item, value] -> sorted [item]

-- (&&&) :: a b c -> a b c' -> a b (c, c') 
-- Fanout: send the input to both argument arrows and combine their output.
-- The default definition may be overridden with a more efficient version if desired.

-- comparing adjacent elements
rises xs = zipWith (<) xs (tail xs)

-- lazy substring search
-- "ell" -> "hello" -> True
substr a b = any (a `isPrefixOf`) $ tails b

-- multiple splitAt's
-- splitAts [2,5,0,3] [1..15] == [[1,2],[3,4,5,6,7],[],[8,9,10],[11,12,13,14,15]]
splitAts = foldr (\n r -> splitAt n >>> second r >>> uncurry (:)) return

-- (>>>) :: Category cat => cat a b -> cat b c -> cat a c
-- Left-to-right composition

-- frequency distribution
-- "abracadabra" -> fromList [('a',5),('b',2),('c',1),('d',1),('r',2)]
import Data.Map
historgram = fromListWith (+) . (`zip` repeat 1)

historgramArr = map (head &&& length) . group . sort


-- multidimensional zipWith
zip2DWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zip2DWith = zipWith . zipWith
zip3DWith :: (a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]
zip3DWith = zipWith . zipWith . zipWith
-- etc.


-- 2. Mathematical sequence, etc

-- combinators
    -- (&) :: a -> (a -> b) -> b infixl 1
        -- reverse application
    -- fix :: (a -> a) -> a
        -- least fixed point of the function (f x = x)
        -- fix f = let x = f x in x
        -- ref: https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
    -- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c infixl 0
        -- g `on` f = \x y -> f x `g` f y.

-- factorial
fact1 n = product [1..n]

fact2 n = foldl 1 (*) [1..n]

-- a :: t -> t
fact3 n = fix (\f n -> if n <= 0 then 1 else n * f(n-1))

-- power of n sequence
powerseq1 n = iterate (*n) 1

powerseq2 n = unfoldr (\z -> Just (z, n*z)) 1

-- fibonacci sequence
fib1 = unfoldr (\(f1, f2) -> Just (f1, (f2, f1+f2))) (0, 1)

fibs = 0:1:zipWith (+) fibs tail (fibs)

fib2 = 0:scanl (+) 1 fib2

-- pascal triangle
pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

-- prime numbers
primes1 = sieve [2..] where
    sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p > 0]

primes2 = sieve [2..] where
    sieve (p:xs) = Just (p, [n | n <- xs, n `mod` p > 0])

primes3 = nubBy (((>1) .) . gcd) [2..]

-- Sieve of Eratosthenes
diff xl@(x:xs) yl@(y:ys) 
    | x < y = x:diff xs yl
    | x > y = diff xl ys
    | otherwise = diff xs ys

eprimes = esieve [2..] where
    esieve (p:xs) = p : esieve (diff xs [p, p+p])

-- or if you want your n primes in less than n^1.5 time instead of n^2.2+
peprimes = 2 : pesieve [3..] peprimes 4 where
    pesieve xs (p:ps) q | (h, t) <- span (<q) xs
        = h ++ pesieve (diff t [q, q+p..]) ps (head ps^2)

-- enumerating the rationals
rats :: [Rational]
rats = iterate next 1 where
    next x = recip (fromInteger n+1-y) where
        (n, y) = properFraction x

rats2 = fix ((1:) . (>>= \x -> [1+x, 1/(1+x)])) :: [Rational]


-- 3. Monad magic

-- all combinations of a list of lists.
-- these solutions are all pretty much equivalent in that they run
-- in the List Monad. the "sequence" solution has the advantage of
-- scaling to N sublists.
-- "12" -> "45" -> ["14", "15", "24", "25"]
allcomb1 :: (Monad m, Traversable t) => t (m a) -> m (t a)
allcomb1 = sequence

allcomb2 :: String -> String -> [String]
allcomb2 a b = [[x, y] | x <- a, y <- b]

allcomb3 a b = do
    x <- a
    y <- b
    return [x, y]

allcomb4 a b =
    a >>= \x ->
        b >>= \y ->
            return [x, y]

-- all combinations of letters
allcombletter = (inits . repeat) ['a'..'z'] >>= sequence

-- apply a list of functions to an argument
-- [a -> b] -> a -> [b]
seqf = sequence
-- or map f 

-- all subsequences of a sequence / powerset
-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
subseq = filterM (const [True, False])

-- apply a function to two other function the same argument
--   (lifting to the Function Monad (->))
-- even 4 && odd 4 -> False

-- liftM2 (&&) f1 f2 a
-- liftM2 (>>) putStrLn return "hello"

-- forward function concatenation
-- (>>>)
-- foldl1 (flip (.))

-- perform functions in/on a monad : lifting
-- fmap
-- liftM2

fac_fib n = mapAccumL (\acc n -> (acc+n,acc+n)) 0 [1..n]


-- 4. Other

-- simulating lisp's cond
cond1 = case () of () 
    | 1 > 2 -> True
    | 3 < 4 -> False
    | otherwise -> True

cond2 = foldr (uncurry if')

-- match a constructor
-- this is better than applying all the arguments, because this way the
-- data type can be changed without touching the code (ideally).
cond3 = case a of 
    Just{} -> True
    _      -> False

-- Polynomial Algebra
instance Num a => Num [a] where
    (f:fs) + (g:gs) = f+g : fs+gs
    fs + [] = fs
    [] + gs = gs

    (f:fs) * (g:gs) = f*g : [f]*gs + fs*(g:gs)
    _ * _ = []

    abs = undefined
    signum = map signum
    fromInteger n = [fromInteger]
    negate = map (\x -> -x)