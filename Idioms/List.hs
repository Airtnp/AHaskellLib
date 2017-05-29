-- List
-- ref: https://wiki.haskell.org/List_function_suggestions
-- ref: https://wiki.haskell.org/List_instance
-- ref: https://wiki.haskell.org/List_notation
-- ref: https://wiki.haskell.org/List_traversal

-- List notation
0 :
1 :
2 : 
[]

-- difference list style
[1,2,3] ++
4 :
listA ++
5 :
listB ++
[]

infixr 5 ?:, ?++
 
(?:) :: (Bool, a) -> [a] -> [a]
(?:) (b, x) = if b then (x:) else id
 
(?++) :: (Bool, [a]) -> [a] -> [a]
(?++) (b, x) = if b then (x++) else id
 
list =
   [2,3] ++
   (x==5, 5) ?:
   (x==7, listA) ?++
   []

-- You can construct a singleton list with a section of the colon operator:
(:[]) :: a -> [a]

-- You can prepend an element to a list:
(x:) :: [a] -> [a]

-- You can extend the scheme by more constructors, as in non-empty package.
data NonEmpty f a = a :! f a
 
infixr 5 :!
 
example :: NonEmpty (NonEmpty []) Int
example = 0 :! 1 :! 2 : 3 : 4 : []

-- You can use the example list in situations where you need to prove that the list contains at least two elements.

-- You can adapt this style to other list-like data structures, e.g. a list of elements with alternating element types. See e.g. event-list.

data Alternating a b = Alternating a [(b,a)]
 
infixr 5 /., ./
 
(/.) :: a -> [(b,a)] -> Alternating a b
(/.) = Alternating
 
(./) :: b -> Alternating a b -> [(b,a)]
b ./ Alternating a bas = (b,a) : bas
 
example :: Alternating Bool Int
example = True /. 0 ./ False /. 1 ./ True /. []

-- List traversal

partitionEithers :: [Either a b] -> ([a], [b])

-- quadratic runtime, not lazy
-- This implementation works for finite lists but fails for infinite ones. You will also notice that it is quite slow. The reason is that appending something to a list like as requires length as steps in order to reach the end of as.

partitionEithers2 :: [Either a b] -> ([a], [b])
partitionEithers2 =
   let aux ab [] = ab
       aux (as, bs) (Left a  : es) = aux (as ++ [a], bs) es
       aux (as, bs) (Right b : es) = aux (as, bs ++ [b]) es
   in  aux ([], [])

-- linear runtime, still not lazy
-- prepending a single element is very cheap, it needs only constant number of operations. reverse is not lazy
partitionEithers1 :: [Either a b] -> ([a], [b])
partitionEithers1 xs =
   let aux ab [] = ab
       aux (as, bs) (Left a  : es) = aux (a : as, bs) es
       aux (as, bs) (Right b : es) = aux (as, b : bs) es
       (ys,zs) = aux ([], []) xs
   in  (reverse ys, reverse zs)

-- linear runtime and full laziness
-- In order to get linear runtime and full laziness we must produce the list in the same order as the input. However we must avoid appending to the end of the list. Instead we must prepend elements to lists that become known in the future. We must be very careful that the leading elements of the result lists can be generated without touching the following elements. Here is the solution:

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (Left a : es) =
   let (as,bs) = partitionEithers es
   in  (a:as, bs)
partitionEithers (Right b : es) =
   let (as,bs) = partitionEithers es
   in  (as, b:bs)

-- The following expressions would match strictly and thus would fail:
--   (\(as,bs) -> (a:as, bs)) $ partitionEithers es
--   case partitionEithers es of (as,bs) -> (a:as, bs)

{-

    The above let can be rewritten equivalently to:
    let ~(as,bs) = partitionEithers es
    in  (a:as, bs)
    (\ ~(as,bs) -> (a:as, bs)) $ partitionEithers es
    case partitionEithers es of ~(as,bs) -> (a:as, bs)
    or without the tilde as syntactic sugar:

    case partitionEithers es of ab -> (a : fst ab, snd ab)
    Of course, both fst and snd
    contain strict pattern matches on the pair constructor but the key difference to above is that these matches happen inside the pair constructor of

    (a : fst ab, snd ab).
    That is, the outer pair constructor can be generated

    before the evaluation of ab is started.

-}

-- Now real experts would not recurse manually but would let foldr do this job. This allows for fusion. Additionally real experts would add the line (\ ~(as,bs) -> (as,bs)) in order to generate the pair constructor of the result completely independent from the input. This yields maximum laziness.

partitionEithersFoldr :: [Either a b] -> ([a], [b])
partitionEithersFoldr =
   (\ ~(as,bs) -> (as,bs)) .
   foldr
      (\e ~(as,bs) ->
         case e of
            Left a -> (a:as, bs)
            Right b -> (as, b:bs))
      ([], [])

-- How to make a list type an instance of some type class in Haskell 2010?
-- Haskell 2010 does not support instances on particular composed types like String.

class C a where
    toX   :: a -> X

-- but not for Strings, given that they are a synonym for [Char]. Hence:
    -- instance C String where toX  = ...  -- ERROR!

class C' a where
    toX     :: a -> X
    listToX :: [a] -> X

-- Answer
newtype IdString = IdString String
-- Then
class C IdString where
    toX = X ()

-- or
-- If you need list functions, then the wrapper approach means that you have to litter your code with calls to the wrapper constructor. In this case you should stick to the original composed type. The trick in the Prelude for the Show class is, that listToX has a default implementation. If this is not possible in your application then introduce a new class like

class Element a where
    listToX :: [a] -> X

instance Element a => C [a] where
    toX = listToX

-- More generally, you can introduce a new class like
class IsChar a where
    fromChar :: Char -> a
    toChar :: a -> Char
instance IsChar Char where
    fromChar = id
    toChar = id

-- FlexibleInstances

{-# LANGUAGE FlexibleInstances #-}
instance C [Char] where toX  = ...


-- Function suggestions Data.List.HT

{-

    First of all: Check out whether the split package provides, what you need.

    We need a few of these:

    split :: Eq a => a -> [a] -> [[a]]
    splitWith :: (a -> Bool) -> [a] -> [[a]]
    tokens :: (a -> Bool) -> [a] -> [[a]]
    That preserve:

    join sep . split sep = id
    See below for 'join'

    And some that use above split but filter to remove empty elements (but do not preserve above property). Easy enough:

    split' :: Eq a => a -> [a] -> [[a]]
    splitWith' :: (a -> Bool) -> [a] -> [[a]]
    tokens' :: (a -> Bool) -> [a] -> [[a]]
    i.e.

    split' sep = filter (not . null) . split sep
    Usage would be:

    tokensws = tokens' (`elem` " \f\v\t\n\r\b")
    
    tokensws "Hello  there\n \n   Haskellers! " ==
    ["Hello", "there", "Haskellers!"]


    replace :: [a] -> [a] -> [a] -> [a]
    like Python replace:

    replace "the" "a" "the quick brown fox jumped over the lazy black dog"
    ===>
    "a quick brown fox jumped over a lazy black dog"

    join (working name)
    join :: [a] -> [[a]] -> [a]
    join sep = concat . intersperse sep
    Note: this function has been implemented as 'intercalate' in Data.List.

    Sorted lists
    The following are versions of standard prelude functions, but intended for sorted lists. The advantage is that they frequently reduce execution time by an O(n). The disadvantage is that the elements have to be members of Ord, and the lists have to be already sorted.

    -- Eliminates duplicate entries from the list, where duplication is defined
    -- by the 'eq' function.  The last value is kept.
    sortedNubBy :: (a -> a -> Bool) -> [a] -> [a]
    sortedNubBy eq (x1 : xs@(x2 : _)) =
        if eq x1 x2 then sortedNubBy eq xs else x1 : sortedNubBy eq xs
    sortedNubBy _ xs = xs
    
    sortedNub :: (Eq a) => [a] -> [a]
    sortedNub = sortedNubBy (==)
    
    -- Merge two sorted lists into a new sorted list.  Where elements are equal
    -- the element from the first list is taken first.
    mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
    mergeBy cmp xs@(x1:xs1) ys@(y1:ys1) =
        if cmp x1 y1 == GT
        then y1 : mergeBy cmp xs ys1
        else x1 : mergeBy cmp xs1 ys
    mergeBy _ [] ys = ys
    mergeBy _ xs [] = xs
    
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge = mergeBy compare

    Generalize groupBy and friends
    
    In the Haskell 98 List library, groupBy assumes that its argument function defines an equivalence, and the reference definition returns sublists where each element is equivalent to the first. The following definition, comparing adjacent elements, does the same thing on equivalence relations:
    groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy rel []          =  []
    groupBy rel (x:xs)      =  (x:ys) : groupBy rel zs
    where (ys,zs) = groupByAux x xs
            groupByAux x0 (x:xs) | rel x0 x = (x:ys, zs)
            where (ys,zs) = groupByAux x xs
            groupByAux y xs = ([], xs)
    However it is also useful on other relations, e.g.

    Picking out maximal ascending sublists (runs):
    > groupBy (<=) [7,3,5,9,6,8,3,5,4]
    [[7],[3,5,9],[6,8],[3,5],[4]]
    Picking out contiguous sublists from an ascending sequence:
    > groupBy (\a b -> a+1 == b) [1,2,3,4,6]
    [[1,2,3,4],[6]]
    Splitting a line at the start of each word:
    > groupBy (\ c1 c2 -> isLetter c1 || not (isLetter c2)) "This is a line"
    ["This ","is ","a ","line"]
    Since this more useful definition agrees with the Haskell 98 one on its specified domain, it should be a backwards-compatible replacement.

    The same applies to nubBy, and possibly deleteBy, deleteFirstsBy and intersectBy (which could have more general types to make this clear).

    groupOn and sortOn
    
    Almost all uses of groupBy and sortBy actually use a specific compare function. This can (using a recent version of base) as sortBy (comparing fst)
    or

    sortBy (compare `on` fst).
    Since this use is so common, it might be worthwhile to add separate functions for this:

    sortOn :: Ord b => (a -> b) -> [a] -> [a]
    sortOn = sortBy . comparing
    The same goes for groupBy
    groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
    groupOn f = groupBy ((==) `on` f)
    That is,

    groupOn' :: Eq b => (a -> b) -> [a] -> [[a]]
    groupOn' f = groupBy (\x y -> f x == f y)
    The names could be better, the idea behind 'on' comes from the 'on' function.

    See utility-ht package.

    Indexing lists

    Find the index of a sublist in a list
    findSublistIndex :: Eq a => [a] -> [a] -> Maybe Int
    findSublistIndex xss xs = findIndex (isPrefixOf xss) $ tails xs
    
    -- Examples
    findSublistIndex findSublistIndex [5,6] [1..] == Just 4
    findSublistIndex "abbc" "abcabbc" == Just 3
    findSublistIndex [2,1] [2,4..10] == Nothing
-}