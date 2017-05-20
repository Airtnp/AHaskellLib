-- fold/foldl as foldr/foldl'
-- ref: https://wiki.haskell.org/Fold
-- ref: https://wiki.haskell.org/Foldr_Foldl_Foldl%27
-- ref: https://wiki.haskell.org/Foldl_as_foldr
-- alt: https://wiki.haskell.org/Foldl_as_foldr_alternative

-- 1. Fold

-- In functional programming, fold (or reduce) is a family of higher order functions that process a data structure in some order and build a return value. This is as opposed to the family of unfold functions which take a starting value and apply it to a function to generate a data structure.

-- Typically, a fold deals with two things: a combining function, and a data structure, typically a list of elements. The fold then proceeds to combine elements of the data structure using the function in some systematic way.

-- non-associative -> foldl vs. foldr

-- tree-like folds

foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
 
foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
 
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t

-- fold -> Church encoding

-- catamorphism

    -- This way of looking at things provides a simple route to designing fold-like functions on other algebraic data structures, like various sorts of trees. One writes a function which recursively replaces the constructors of the datatype with provided functions, and any constant values of the type with provided values. Such functions are generally referred to as Catamorphisms.

-- 2. foldr foldl foldl'

-- We somehow have to tell the system that the inner redex should be reduced before the outer. Or the stack will overflow by lazy-evaluation or foldr

-- seq :: a -> b -> b
-- seq is a primitive system function that when applied to x and y will first reduce x then return y. The idea is that y references x so that when y is reduced x will not be a big unreduced chain anymore.

foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x 
                    in seq z' $ foldl' f z' xs

-- Usually the choice is between foldr and foldl', since foldl and foldl' are the same except for their strictness properties, so if both return a result, it must be the same. foldl' is the more efficient way to arrive at that result because it doesn't build a huge thunk. However, if the combining function is lazy in its first argument, foldl may happily return a result where foldl' hits an exception:

    -- Rules of Thumb for Folds

    {-
        Folds are among the most useful and common functions in Haskell. They are an often-superior replacement for what in other language would be loops, but can do much more. Here are a few rules of thumb on which folds to use when.

        foldr is not only the right fold, it is also most commonly the right fold to use, in particular when transforming lists (or other foldables) into lists with related elements in the same order. Notably, foldr will be effective for transforming even infinite lists into other infinite lists. For such purposes, it should be your first and most natural choice. For example, note that foldr (:) []==id.

        Note that the initial element is irrelevant when foldr is applied to an infinite list. For that reason, it is may be good practice when writing a function which should only be applied to infinite lists to replace foldr f [] with foldr f undefined. This both documents that the function should only be applied to infinite lists and will result in an error when you try to apply it to a finite list.

        The other very useful fold is foldl'. It can be thought of as a foldr with these differences:

            foldl' conceptually reverses the order of the list. One consequence is that a foldl' (unlike foldr) applied to an infinite list will be bottom; it will not produce any usable results, just as an express reverse would not. Note that foldl' (flip (:)) []==reverse.

            foldl' often has much better time and space performance than a foldr would for the reasons explained in the previous sections.

        You should pick foldl' principally in two cases:

            When the list to which it is applied is large, but definitely finite, you do not care about the implicit reversal (for example, because your combining function is commutative like (+), (*), or Set.union), and you seek to improve the performance of your code.

            When you actually do want to reverse the order of the list, in addition to possibly performing some other transformation to the elements. In particular, if you find that you precede or follow your fold with a reverse, it is quite likely that you could improve your code by using the other fold and taking advantage of the implicit reverse.

        foldl is rarely the right choice. It gives you the implicit reverse of fold, but without the performance gains of foldl'. Only in rare, or specially constructed cases like in the previous section, will it yield better results than foldl'.

        Another reason that foldr is often the better choice is that the folding function can short-circuit, that is, terminate early by yielding a result which does not depend on the value of the accumulating parameter. When such possibilities arise with some frequency in your problem, short-circuiting can greatly improve your program's performance. Left folds can never short-circuit.
    
    -}

-- 3. foldl as foldr

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a bs =
   foldr (\b g x -> g (f x b)) id bs a

foldl f a list = go2 list a
  where
    go2 [] = (\acc -> acc)                      -- nil case
    go2 (x : xs) = \acc -> (go2 xs) (f acc x)   -- construct x (go2 xs)

-- By paper: http://www.cs.nott.ac.uk/~pszgmh/fold.pdf

go2 list = foldr construct (\acc -> acc) list
  where
    construct x r = \acc -> r (f acc x)

foldl f a list = (foldr construct (\acc -> acc) list) a
  where
    construct x r = \acc -> r (f acc x)

-- Because r is the same function as constructed by the construct here, calling this e.g. for a list [x,y,...,z] scans through the whole list as-if evaluating a nested lambda applied to the initial value of the accumulator,

    {-
        (\acc-> 
            (\acc-> 
                (... (\acc-> (\acc -> acc)
                            (f acc z)) ...)
                (f acc y))
            (f acc x)) a
        
        which creates the chain of evaluations as in

        (\acc -> acc) (f (... (f (f a x) y) ...) z)
-   } 




-- The converse is not true, since foldr may work on infinite lists, which foldl variants never can do. However, for finite lists, foldr can also be written in terms of foldl (although losing laziness in the process), in a similar way like this:

foldr :: (b -> a -> a) -> a -> [b] -> a
foldr f a bs =
   foldl (\g b x -> g (f b x)) id bs a

-- I find it easier to imagine a fold as a sequence of updates. An update is a function mapping from an old value to an updated new value.

newtype Update a = Update {evalUpdate :: a -> a}

instance Monoid (Update a) where
   mempty = Update id
   mappend (Update x) (Update y) = Update (y.x)

foldlMonoid :: (a -> b -> a) -> a -> [b] -> a
foldlMonoid f a bs =
   flip evalUpdate a $
   mconcat $
   map (Update . flip f) bs

-- mconcat :: Monoid a => [a] -> a
-- mconcat = foldr mappend mempty

-- Update a is just Dual (Endo a)
-- If you use a State monad instead of a monoid, you obtain an alternative implementation of mapAccumL.


{-
    Using the foldr expression we can write variants of foldl that behave slightly different from the original one.

    E.g. we can write a foldl that can stop before reaching the end of the input list and thus may also terminate on infinite input.
-}

foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe f a bs =
   foldr (\b g x -> f x b >>= g) Just bs a

import Control.Monad ((>=>), )
 
newtype UpdateMaybe a = UpdateMaybe {evalUpdateMaybe :: a -> Maybe a}
 
instance Monoid (UpdateMaybe a) where
   mempty = UpdateMaybe Just
   mappend (UpdateMaybe x) (UpdateMaybe y) = UpdateMaybe (x>=>y)
 
foldlMaybeMonoid :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybeMonoid f a bs =
   flip evalUpdateMaybe a $
   mconcat $
   map (UpdateMaybe . flip f) bs

-- Parsing numbers using a bound

readBounded :: Integer -> String -> Maybe Integer
readBounded bound str =
   case str of
      ""  -> Nothing
      "0" -> Just 0
      _ -> foldr
         (\digit addLeastSig mostSig ->
            let n = mostSig*10 + toInteger (Char.digitToInt digit)
            in  guard (Char.isDigit digit) >>
                guard (not (mostSig==0 && digit=='0')) >>
                guard (n <= bound) >>
                addLeastSig n)
         Just str 0
 
readBoundedMonoid :: Integer -> String -> Maybe Integer
readBoundedMonoid bound str =
   case str of
      ""  -> Nothing
      "0" -> Just 0
      _ ->
         let m digit =
               UpdateMaybe $ \mostSig ->
                  let n = mostSig*10 + toInteger (Char.digitToInt digit)
                  in  guard (Char.isDigit digit) >>
                      guard (not (mostSig==0 && digit=='0')) >>
                      guard (n <= bound) >>
                      Just n
         in  evalUpdateMaybe (mconcat $ map m str) 0