-- A tutorial on the universality and expressiveness of fold
-- TODO: https://jeremykun.com/tag/foldr/

-- foldr
-- f x acc
fold :: (a -> b -> b) -> b -> [a] -> b
fold f v [] = v
fold f v (x:xs) = f x (fold f v xs)

-- foldl f v (x:xs) = f (fold f v xs) x
-- f acc n

product = fold (*) 0
and = fold (&) True
or = fold (|) False

(++) :: [a] -> [a] -> [a]
(++ ys) = fold (:) ys

length = fold (\x n -> n + 1) 0
reverse = fold (\x xs -> xs ++ [x]) 0
map f = fold (\x xs -> f x : xs) []
filter p = fold (\x xs -> if p x then x : xs else xs) []

-- recursion theory

-- universal property (definition)
-- g [] = v
-- g (x:xs) = f x (g xs)    <=>    g = fold f v

    -- (+1) . sum = fold (+) 1
    
    -- (+1) . sum [] = 1
    -- (+1) . sum (x : xs) = (+) x (((+1) . sum) xs)

-- fusion property
-- h w = v
-- h (g x y) = f x (h y)    <=>    h . fold g w = fold f v

    -- (+1) . sum = fold (+) 1
    -- (+1) . fold (+) 0 = fold (+) 1

        -- (+1) 0 = 1
        -- (+1) ((+) x y) = (+) x ((+1) y)

    -- infix n op
    -- (op a) . fold (op) b = fold (op) (b op a)

    -- map f = fold (\x xs -> f x : xs) []
    -- map f [] = []
    -- map f (g x : y) = (f . g) x : map f y

-- generate tuples
-- sum and length
sumLength :: [Int] -> (Int, Int)
-- sumLength xs = (sum xs, length xs)
sumLength = fold (\n (x, y) -> (n + x, 1 + y)) (0, 0)

-- cannot be implemented as fold because xs is stateless
dropWhile :: (a -> Bool) -> [a] -> [a]
-- using tupling techniques we can redefine it
dropWhile' :: (a -> Bool) -> ([a] -> ([a], [a]))
dropWhile' p = fold f v where
    f x (ys, xs) = (if p x then ys else x : xs, x : xs)
    v            = ([], [])

-- primitive recursion

-- h y [] = f y
-- h y (x:xs) = g y x (h y xs)
-- h y = fold (g y) (f y)

-- primitive recursion
-- h y [] = f y
-- h y (x:xs) = g y x xs (h y xs)

-- k y [] = (f y, [])
-- k y xs = (h y xs, xs)
-- k y [] = j
-- k y (x:xs) = i x (k y xs)
-- k y = fold i j where
    -- i x (z, xs) = (g y x xs, x : xs)
    -- j           = (f y, [])

-- h y = fst . k y


-- generate functions
compose :: [a -> a] -> a -> a
compose = fold (.) id
-- right to left
sum :: [Int] -> Int
sum = fold (+) 0
suml :: [Int] -> Int
suml xs = suml' xs 0 where
    suml' [] n = n
    suml' (x:xs) n = suml' xs (n + x)
    -- suml' :: [Int] -> Int -> Int
    -- suml' = fold (\x g -> (\n -> g (n + x))) id
-- suml xs = fold (\x g -> (\n -> g (n + x))) id xs 0

foldl f v xs = fold (\x g -> (\a -> g (f a x))) id xs v

ack :: [Int] -> [Int] -> [Int]
ack [] ys = 1 : ys
ack (x:xs) [] = ack xs [1]
ack (x:xs) (y:ys) = ack xs (ack (x:xs) ys)

-- ack = fold (\x g -> fold (\y -> g) ( g [1] )) (1 :)

-- Other works
    -- fold for regular datatypes (not just lists)
    -- fold for nested datatypes
    -- fold for functional datatypes
    -- Monadic fold
    -- Relational fold
    -- other reursion operators
    -- automatic program transformation
    -- polytypic programming
    -- programming languages