-- Zipper
-- ref: https://wiki.haskell.org/Zipper
-- ref: https://wiki.haskell.org/Zipper_monad
-- oleg: http://okmij.org/ftp/continuations/zipper.html
-- wikibook: https://en.wikibooks.org/wiki/Haskell/Zippers
-- blog: http://blog.sigfpe.com/2007/01/monads-hidden-behind-every-zipper.html

-- The Zipper is an idiom that uses the idea of “context” to the means of manipulating locations in a data structure. Zipper monad is a monad which implements the zipper for binary trees.

data Tree a = Fork (Tree a) (Tree a) | Leaf a


t = Fork (Fork (Leaf 1)
               (Leaf 2))
         (Fork (Leaf 3)
               (Leaf 4))

-- Each subtree of this tree occupies a certain location in the tree taken as a whole. The location consists of the subtree, along with the rest of the tree, which we think of the context of that subtree. For example, the context of Leaf 2 is

Fork (Fork (Leaf 1) @)
     (Fork (Leaf 3) (Leaf 4))

-- where @ marks a hole: the spot that the subtree appears in. This is the way we shall implement a tree with a focus. One way of expressing this context is as a path from the root of the tree to the hole (to which the required subtree will be attached). To reach our subtree, we needed to go down the left branch, and then down the right one. Note that the context is essentially a way of representing the tree, “missing out” a subtree (the subtree we are interested in).

-- A naive implementation of the context, inspired directly by the graphical representation might be to use Tree (Maybe a) instead of Tree a. However, this would lose an essential point: in any given context, there is exactly one hole. Therefore, we will represent a context as follows:

data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a)

-- L c t represents the left part of a branch of which the right part was t and whose parent had context c. The R constructor is similar. Top represents the top of a tree.

{-

    In fact, this is equivalent to a list, whose elements are the appropriate collateral trees, each element labeled with the information which direction was chosen:
    type Context a = [(Direction, Tree a)]
    data Direction = Lft | Rght

    We chose to propagate from the hole towards the root. This is an independent idea of the above considerations: it is not the unicity of the hole that forced us to do so. It is simply more efficient if we want to define operations later for a tree with a focus (move focus left, right, up).

    Note that in the original paper, Huet dealt with B-trees (ones where nodes have arbitrary numbers of branches), so at each node, a list is used instead of the two (Tree a) parameters to represent children. Later we shall see that the solution of the problem can be formalized in a general way which covers solutions for both kinds of trees, as special cases.

-}

-- Using this datatype, we can rewrite the sample context above in proper Haskell:

R (Leaf 1) (L Top (Fork (Leaf 3) (Leaf 4)))

-- Now we can define a tree location:

type Loc a = (Tree a, Cxt a)

-- thus, a tree with a focus (drawn here as a tree with a marked subtree) shall be represented as “mounting” the focus (a tree) into the hole of the appropriate context.

-- Now, we can define some useful functions for manipulating locations in a tree:

left :: Loc a -> Loc a
left (Fork l r, c) = (l, L c r)
 
right :: Loc a -> Loc a
right (Fork l r, c) = (r, R l c)
 
top :: Tree a -> Loc a
top t = (t, Top)
 
up :: Loc a -> Loc a
up (t, L c r) = (Fork t r, c)
up (t, R l c) = (Fork l t, c)
 
upmost :: Loc a -> Loc a
upmost l@(t, Top) = l
upmost l = upmost (up l)
 
modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t, c) f = (f t, c)

-- It is instructive to look at an example of how a location gets transformed as we move from root to leaf. Recall our sample tree t. Let's name some of the relevant subtrees for brevity:

t = let tl = Fork (Leaf 1) (Leaf 2)
        tr = Fork (Leaf 3) (Leaf 4)
    in Fork tl tr

-- Then to reach the location of Leaf 2:

(right . left . top) t
= (right . left) (t, Top)
= right (tl, L Top tr)
= (Leaf 2, R (Leaf 1) (L Top tr))

-- To reach that location and replace Leaf 2 by Leaf 0:

modify ((right . left . top) t) (\_ -> Leaf 0)
= ...
= (Leaf 0, R (Leaf 1) (L Top tr))

-- Afterwards some may like to continue walking to other parts of the new tree, in which case continue applying left, right, and up.

-- Some others may like to retrieve the new tree (and possibly forget about locations), in which case upmost is useful:

(fst . upmost) (modify ((right . left . top) t) (\_ -> Leaf 0))
= (fst . upmost) (Leaf 0, R (Leaf 1) (L Top tr))
= fst (Fork (Fork (Leaf 1)
                  (Leaf 0))
            tr
      , Top)
= Fork (Fork (Leaf 1)
             (Leaf 0))
       tr

{-

    Automation

        There's a principled way to get the necessary types for contexts and the context filling functions, namely by differentiating the data structure. Some relevant papers.

        For an actual implementation in Generic Haskell, see the paper Type-indexed data types by Ralf Hinze, Johan Jeuring and Andres Löh, or a similar paper Generic Haskell: Applications by Ralf Hinze and Johan Jeuring

    Alternative formulation

        The dual of Huet zipper is generic zipper -- which is a derivative of a traversal function rather than that of a data structure. Unlike Huet zipper, generic zipper can be implemented once and for all data structures, in the existing Haskell. Generic Zipper and its applications

    Comonads and monads



-}

-- Zipper Monad

-- The Zipper Monad is a generic monad for navigating around arbitrary data structures. It supports movement, mutation and classification of nodes (is this node the top node or a child node?, etc). It was proposed and designed by Paolo Martini (xerox), and coded by David House (davidhouse). It's designed for use with The Zipper but in fact there is no requirement to use such an idiom.


-- At the moment there are two specific libraries that use the Travel monad: TravelTree for navigating around binary trees, and TravelBTree for navigating around "B-Trees", trees where each node has an arbitrary number of branches. Please see below for an alternative zipper implementation that works for any data structure whatsoever.

data Loc c a = Loc { struct :: a,
                     cxt    :: c }
             deriving (Show, Eq)
 
newtype Travel loc a = Travel { unT :: State loc a }
     deriving (Functor, Monad, MonadState loc, Eq)

-- Computations in Travel are stateful. Loc c a is a type for storing the location within a structure. struct should be the substructure that the Loc is refering to, and cxt the "context" of the substructure; i.e. the rest of the structure. Loc is designed to hold a Zipper (although it doesn't have to; for example if you wanted to traverse a list it would probably be more natural to hold the entire structure and an index). Indeed, both of the libraries provided with the generic Travel monad use a zipper.

{-

Functions

    Movement

    At the moment, movement is specific to the structure you are traversing and as such, the movement functions are provided by libraries implementing specific structures. Try the documentation for TravelTree (binary trees) or TravelBTree (B-Trees; trees where each node has an arbitrary number of branches).

    Mutation

    There are three generic functions available for changing the structure:

    getStruct    :: Travel (Loc c a) a
    putStruct    :: a -> Travel (Loc c a) a
    modifyStruct :: (a -> a) -> Travel (Loc c a) a
    These are direct front-doors for State's get, put and modify, and all three return the substructure after any applicable modifications.
    2.3 Exit points
    To get out of the monad, use traverse:
    traverse :: Loc c a            -- starting location (initial state)
            -> Travel (Loc c a) a -- locational computation to use
            -> a                  -- resulting substructure
    Again, this is just a front-door for evalState. Note that you have to give a Loc as a starting state. Both the libraries provided supply a getTop function, which takes a tree and returns the Loc corresponding to the top of the tree. Thus a typical call to traverse might look like:
    let t = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
    in (getTop t) `traverse` (left >> swap >> right)

-}



{-# OPTIONS_GHC -fglasgow-exts #-}
module Zipper where
 
-- A monad implementing for traversing data structures
-- http://haskell.org/haskellwiki/Zipper_monad
--------------------------------------------------------------------------------
 
import Control.Monad.State
 
data Loc c a = Loc { struct :: a,
                     cxt    :: c }
             deriving (Show, Eq)
 
newtype Travel loc a = Travel { unT :: State loc a }
     deriving (Functor, Monad, MonadState loc, Eq)
 
-- Exit Points
--
 
-- get out of the monad
traverse :: Loc c a            -- starting location (initial state)
         -> Travel (Loc c a) a -- locational computation to use
         -> a                  -- resulting substructure
traverse start tt = evalState (unT tt) start
 
-- Mutation
-- 
 
-- modify the substructure at the current node
modifyStruct :: (a -> a) -> Travel (Loc c a) a
modifyStruct f = modify editStruct >> liftM struct get where
    editStruct (Loc s c) = Loc (f s) c
 
-- put a new substructure at the current node
putStruct :: a -> Travel (Loc c a) a
putStruct t = modifyStruct $ const t
 
-- get the current substructure
getStruct :: Travel (Loc c a) a
getStruct = modifyStruct id -- works because modifyTree returns the 'new' tree

-- TravelBTree/TravalTree