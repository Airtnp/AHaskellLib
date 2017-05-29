-- Memoization
-- ref: https://wiki.haskell.org/Memoization

-- 1. Memoization without recursion
-- http://conal.net/blog/tag/memoization

memoize :: Memoizable a => (a -> b) -> (a -> b)

Map ()            b  := b
Map (Either a a') b  := (Map a b, Map a' b)
Map (a,a')        b  := Map a (Map a' b)

-- () -> b  =~=  b
-- (a + a') -> b  =~=  (a -> b) × (a' -> b) -- = case analysis
-- (a × a') -> b  =~=  a -> (a' -> b)       -- = currying

-- 2. Memoization with recursion

slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . slow_fib)

{-

fibMemo = fix (memoize . fib)
        = let x = (memoize . fib) x in x
        = (memoize . fib) fibMemo
        = memoize (fib fibMemo)

fibMemo 3
        = memoize (fib fibMemo) 3
        = map (fib fibMemo) [0 ..] !! 3
        -- Note: The following step does not exactly follow lazy evaluation.
        = fib fibMemo 0 : fib fibMemo 1 : fib fibMemo 2 : fib fibMemo 3 : map (fib fibMemo) [4 ..] !! 3
        = fib fibMemo 3
        = fibMemo 2 + fibMemo 1
        = fib fibMemo 2 + fibMemo 1
        = (fibMemo 1 + fibMemo 0) + fibMemo 1
        -- Note: Because of the memoization, both “fibMemo 1” terms refer to the same thunk,
        --       so it will only be evaluated once.
        = (fib fibMemo 1 + fibMemo 0) + fibMemo 1
        = (1 + fibMemo 0) + fibMemo 1
        = (1 + fib fibMemo 0) + fibMemo 1
        = (1 + 0) + fibMemo 1
        = 1 + fibMemo 1
        -- Remember: “fibMemo 1” was already evaluated, so we can directly replace it by its value.
        = 1 + 1
        = 2

-}

-- 3. Efficient tree data structure for maps from Int to somewhere
-- data-inttrie

-- Fibonacci function
memoizeInt :: (Int -> a) -> (Int -> a)
memoizeInt f = (fmap f (naturals 1 0) !!!)

data NaturalTree a = Node a (NaturalTree a) (NaturalTree a)

Node a tl tr !!! 0 = a 
Node a tl tr !!! n =
   if odd n
     then tl !!! top
     else tr !!! (top-1)
        where top = n `div` 2

instance Functor NaturalTree where
   fmap f (Node a tl tr) = Node (f a) (fmap f tl) (fmap f tr)

naturals = Node 0  (fmap ((+1).(*2)) naturals) (fmap ((*2).(+1)) naturals)

naturals r n =
   Node n
     ((naturals $! r2) $! (n + r))
     ((naturals $! r2) $! (n + r2))
        where r2 = 2 * r

-- 4. Memoising CAFS

-- Memoising constructor functions gives you HashConsing, and you can sometimes use MemoisingCafs (constant applicative forms) to implement that. The MemoisingCafs idiom also supports recursion.


-- 5. Memoizing polymorphic functions
-- ref: http://conal.net/blog/posts/memoizing-polymorphic-functions-part-one
-- ref: http://conal.net/blog/posts/memoizing-polymorphic-functions-part-two
-- ref: http://conal.net/blog/posts/memoizing-polymorphic-functions-via-unmemoization

-- What about memoizing polymorphic functions defined with polymorphic recursion? How can such functions be memoized? The caching data structures used in memoization typically handle only one type of argument at a time. For instance, one can have finite maps of differing types, but each concrete finite map holds just one type of key and one type of value.
