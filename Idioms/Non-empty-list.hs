-- Non-empty list
-- ref: https://wiki.haskell.org/Non-empty_list

-- Errors such as taking head or tail of the empty list in Haskell are equivalent to the dereferencing of the zero pointer in C/C++ or NullPointerException in Java. These errors occur because the true domain of the function is smaller than the function's type suggests. For example, the type of head says that the function applies to any list. In reality, it can only be meaningfully applied to non-empty lists. One can eliminate such errors by giving functions head and tail more precise type, such as FullList a. Languages like Cyclone and Cw do exactly that.

-- It must be emphasized that we can eliminate head-of-empty-list errors now, without any modification to the Haskell type system, without developing any new tool. In fact, it is possible in Haskell98! The same technique applies to OCaml and even Java and C++. The only required advancement is in our thinking and programming style.

-- Safe list functions

{-# Haskell98! #-}
-- Safe list functions
 
module NList (FullList,
              fromFL,
              indeedFL,
              decon,
              head,
              tail,
              Listable (..)
              ) where
 
import Prelude hiding (head, tail)
 
newtype FullList a = FullList [a]  -- data constructor is not exported!
 
fromFL (FullList x) = x                 -- Injection into general lists
 
-- The following is an analogue of `maybe'
indeedFL :: [a] -> w -> (FullList a -> w) -> w
indeedFL x on_empty on_full 
    | null x = on_empty
    | otherwise = on_full $ FullList x
 
-- A possible alternative, with an extra Maybe tagging
-- indeedFL :: [a] -> Maybe (FullList a)
 
-- A more direct analogue of `maybe', for lists
decon :: [a] -> w -> (a -> [a] -> w) -> w
decon []    on_empty on_full = on_empty
decon (h:t) on_empty on_full = on_full h t
 
 
-- The following are _total_ functions
-- They are guaranteed to be safe, and so we could have used
-- unsafeHead# and unsafeTail# if GHC provides though...
 
head :: FullList a -> a
head (FullList (x:_)) = x
 
tail :: FullList a -> [a]
tail (FullList (_:x)) = x
 
-- Mapping over a non-empty list gives a non-empty list
instance Functor FullList where
    fmap f (FullList x) = FullList $ map f x
 
 
-- Adding something to a general list surely gives a non-empty list
infixr 5 !:
 
class Listable l where
    (!:) :: a -> l a -> FullList a
 
instance Listable [] where
    (!:) h t = FullList (h:t)
 
instance Listable FullList where
    (!:) h (FullList t) = FullList (h:t)

-- Integrating with the existing list-processing functions

{-

    \l -> indeedFL l onempty $  
	       \l -> [head l, indeedFL (foo $ fromFL l) 
				(error msg)
			         head]
	  where msg = "I'm quite sure foo maps non-empty lists to " ++
	              "non-empty lists. I'll be darned if it doesn't."

-}

-- Reliable and simple approach

type NonEmptyList a = (a, [a])

-- Or.. DT in Intro/concepts