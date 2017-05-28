-- ADT: abstract data type/algebraic data type
-- ref: https://wiki.haskell.org/ADT

-- abstract data type
-- An abstract data type is a type with associated operations, 
-- but whose representation is hidden.
-- parameterized data type

data Tree a = Nil | Node { left :: Tree a, value :: a, right :: Tree a }

three_num_tree :: Tree Integer
three_num_tree = Node (Node Nil 1 Nil) 2 (Node Nil 3 Nil)

class TreeT t where
    nil   :: t a
    node  :: t a -> a -> t a -> t a
    left  :: (MonadPlus m) => t a -> m (t a)
    right :: (MonadPlus m) => t a -> m (t a)
    value :: (MonadPlus m) => t a -> m a

empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 

newtype Stack a = StackImpl [a]

empty = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)

-- algebraic data type
-- wiki: https://en.wikipedia.org/wiki/Algebraic_data_type

-- "sum" is alternation (A | B, meaning A or B but not both)
-- "product" is combination (A B, meaning A and B together)

-- concrete data type/concrete view
-- Opposite to ADT


-- generalised algebraic data type
-- GADT
-- ref: https://wiki.haskell.org/Generalised_algebraic_datatype
-- video: http://apfelmus.nfshost.com/blog/2010/06/01-gadts-video.html
-- wikibook: https://en.wikibooks.org/wiki/Haskell/GADT

{-
    Generalised Algebraic Datatypes (GADTs) are datatypes 
        for which a constructor has a non standard type. 
    Indeed, in type systems incorporating GADTs, 
        there are very few restrictions on the type 
        that the data constructors can take. 
-}

data TermSimple = K | S | TermSimple :@ TermSimple 
infixl 6 :@

-- with GADT
-- we can have the terms carry around more type information
-- and create more interesting terms, like so:
-- gadt是injective的 是因为有具体的构造函数/tag在那里。 
-- typecheck遇到数据类型可以推断左侧需要的数据的类型以及需要的约束
-- SKI combinator
data Term x where
    K :: Term (a -> b -> a)
    S :: Term ((a -> b -> c)  -> (a -> b) -> a -> c)
    Const :: a -> Term a
    (:@) :: Term (a -> b) -> (Term a) -> Term b
infixl 6 :@

eval::Term a -> Term a
eval (K :@ x :@ y) = x
eval (S :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
eval x = x

-- Parsing Example

data Parser tok a where
    Zero :: Parser tok ()
    One :: Parser tok ()
    Check :: (tok -> Bool) -> Parser tok tok
    Satisfy :: ([tok] -> Bool) -> Parser tok [tok]
    Push :: tok -> Parser tok a -> Parser tok a
    Plus :: Parser tok a -> Parser tok b -> Parser tok (Either a b)
    Times :: Parser tok a -> Parser tok b -> Parser tok (a,b)
    Star :: Parser tok a -> Parser tok [a]

parse :: Parser tok a -> [tok] -> Maybe a
 
-- Zero always fails.
parse Zero ts = mzero
 
-- One matches only the empty string.
parse One [] = return ()
parse One _  = mzero
 
-- Check p matches a string with exactly one token t such that p t holds.
parse (Check p) [t] = if p t then return t else mzero
parse (Check p) _ = mzero
 
-- Satisfy p any string such that p ts holds.
parse (Satisfy p) xs = if p xs then return xs else mzero
 
-- Push t x matches a string ts when x matches (t:ts).
parse (Push t x) ts = parse x (t:ts)
 
-- Plus x y matches when either x or y does.
parse (Plus x y) ts = liftM Left (parse x ts) `mplus` liftM Right (parse y ts)
 
-- Times x y matches the concatenation of x and y.
parse (Times x y) [] = liftM2 (,) (parse x []) (parse y [])
parse (Times x y) (t:ts) = 
    parse (Times (Push t x) y) ts `mplus`
    liftM2 (,) (parse x []) (parse y (t:ts))
 
-- Star x matches zero or more copies of x.
parse (Star x) [] = return []
parse (Star x) (t:ts) = do
    (v,vs) <- parse (Times x (Star x)) (t:ts)
    return (v:vs)

token x = Check (== x)
string xs = Satisfy (== xs)