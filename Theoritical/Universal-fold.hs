-- A tutorial on the universality and expressiveness of fold

fold :: (a -> b -> b) -> b -> [a]
fold f v [] = v
fold f v (x:xs) = f x (fold f v xs)

sum = fold (+) 0
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
-- g [] = v
-- g (x:xs) = f x (g xs)    <=> g = fold f v