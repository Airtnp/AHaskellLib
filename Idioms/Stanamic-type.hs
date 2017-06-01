-- Stanamic typing
-- ref: https://wiki.haskell.org/Stanamic_typing

-- "We describe a datatype of polymorphic balanced binary trees: AVL trees. The trees are polymorphic: the values in different nodes may have different type. The trees are balanced: for each non-leaf node, the heights of its two children can differ at most by one. Here, by definition the height of a node is 1 + max of the heights of its children. A leaf node has the height of 0.

-- The main feature of the present approach is a blended static and dynamic enforcement of the balancing constraint. The function make_node verifies the balancing constraint at compile time -- if it can. If the static check is not possible, the function delays the check till the run-time"

{-

A datatype of polymorphic stanamically balanced binary trees

We present a datatype of polymorphic binary trees. Nodes within a
single tree can have values of arbitrary types. The tree data
structure is subjected to a balancing constraint: for each non-leaf
node, the heights of its two children can differ at most by one.
Here, by definition the height of a node is 1 + max of the heights of
its children. A leaf node has the height of 0.

The main feature of the present approach is a blended static and
dynamic enforcement of the balancing constraint. Whenever possible, the
typechecker verifies the constraint and flags an attempt to make an
unbalanced tree as a type error. A user cannot construct an unbalanced
tree by composing make_leaf and make_node constructors.

This static enforcement of a dependent type constraint is
good. However, it's obviously limited. To verify type constraints, the
compiler needs to see the entire chain of constructor
applications. That chain however may not be not known until the run
time -- and it can depend on external conditions. How can we guarantee
that a program that builds binary trees from user input data will not
return an unbalanced tree? How can we typecheck a function that builds
a tree recursively, with the recursion depth unknown until the
run-time. Obviously, a run-time check is needed.

The following code shows a simple approach to a combined static and
dynamic checking. By default, a static check is enforced. If the user
cannot placate the typecheker, who keeps complaining about infinite
types, the user can delay the check until the run time, by staging the
typecheck. We enclose the dependent type into an existential envelope
to be opened and checked later. To the end user, all these
machinations are more or less transparent. To build nodes, the user
will invoke the same function, make_node. The latter does a static
check, if it can. Otherwise, it defers the check till the run time.

Another feature of the following code is an undefined arithmetics. We
can increment and compare undefined values, provided they are
appropriately typed.  In fact, the height of our tree data structure
is represented by bottom. The following snippet from a uvaluator
demonstrates an undefined type subtraction:

	instance (UNUM u) => UNUM (Succ u) where
	    uval _ = 1 + (uval (undefined::u))

Finally, yet another feature of the code is polymorphic recursion, which
specifically bypasses the monomorphic restriction.

I should point out a particularly apt error message that I received
from GHCi:

    langs/Haskell/statically-balanced-trees.hs:63:
    My brain just exploded.
    I can't handle pattern bindings for existentially-quantified constructors.
    In the binding group
    (BU (cv :: ct)) = check (uval au) (uval bu)
    In the definition of `heightc':
    let (BU (cv :: ct)) = check (uval au) (uval bu)
    in BU $ (undefined :: Succ ct)

It happened around midnight, when I was quite exhausted already. I
typed ":r", read the first line of the message "My brain just
exploded." and said, "Yeah, tell me about it...". And then I jumped
off the chair, having realized what I've been talking with. I
replaced 'let' with 'case' and the error disappeared. Incidentally,
this code gives another example of the undefined arithmetics:
incrementing the type of an existential bottom.

The literate Haskell code follows. To run it, do
   ghci -fglasgow-exts -fallow-undecidable-instances /tmp/code-file.lhs


To verify the balancing condition at compile type, we need an
appropriate arithmetic type. We chose the following. We should stress
that Zero and Succ are type constructors. There are no data
constructors. In fact, those types have no values except the bottom.

> data Zero
> data Succ a
> type One = Succ Zero
>
> class UNUM u where
>    uval:: u -> Int

Note that uval converts a type to a value.  We can apply uval to
undefined and get the right answer, provided that undefined is typed
appropriately.

> instance UNUM Zero where
>    uval _ = 0
    
> instance (UNUM u) => UNUM (Succ u) where
>    uval _ = 1 + (uval (undefined::u))

The following datatype BU is a run-time representation of UNUMs. All
UNUM types have the same value: the bottom. Therefore, the BU value
represents UNUM's type without the value of the latter. We might
suppose that BU is quite compact: it only needs to keep track of the
enclosed type without worrying about the enclosed value.

> data BU = forall u. (UNUM u) => BU u

Incidentally, the following function umake is the inverse of uval.
The function uval converts a UNUM type to an Int value. The function
umake goes the other way around: from values to types. The function
demonstrates the type arithmetics in a value form. We create and
deconstruct types, whereas the value remains the same: undefined.  We
should also point out that umake implements a polymorphic recursion,
getting around the monomorphic restriction.

> umake 0 = BU (undefined::Zero)
> umake n = case (umake (n-1)) of (BU (x::z)) -> BU $ (undefined::(Succ z))
> --test: case (umake 5) of (BU x) -> uval x

In a follow-up, Tom Pledger has suggested to memoize the results of
umake:
    umade = BU (undefined::Zero)
         : [BU (undefined::Succ z) | BU (x::z) <- umade]
    umake n = umade!!n



The following class HeightC expresses the balancing constraint.  The
class and its instances do both static and dynamic checks.

> class HeightC lheight rheight pheight | lheight rheight -> pheight where
>      heightc:: lheight -> rheight -> (Bool,pheight)
>      heightc a b = (True,(undefined::pheight))

> -- The static portion of the constraint
> instance HeightC Zero Zero (Succ Zero)
> instance HeightC (Succ h) (Succ h) (Succ (Succ h))
> instance HeightC h (Succ h) (Succ (Succ h))
> instance HeightC (Succ h) h (Succ (Succ h))

The following instance does a dynamic check. It opens existential
envelopes with deferred types, does the check, and puts the result
back into the envelope. In a manner of speaking, we stage the
typecheck.

> instance HeightC BU BU BU where
>     heightc a@(BU au) b@(BU bu) = 
>          case  check (uval au) (uval bu) of
> 	    (BU (cv::ct)) -> (True, BU $ (undefined::(Succ ct)))
>       where
>         check av bv | av == bv = a
> 	  check av bv | av == (bv + 1) = a
> 	  check av bv | bv == (av + 1) = b

The following declarations introduce the polymorphic tree datatype.

> data Nil = Nil deriving Show
> data Leaf v h l r = Leaf v h
> data Node v h l r = Node v h l r
> 
> class BBTree ntype vtype ht lchildtype rchildtype where
>     left::    ntype vtype ht lchildtype rchildtype -> lchildtype
>     right::   ntype vtype ht lchildtype rchildtype -> rchildtype
>     is_leaf:: ntype vtype ht lchildtype rchildtype -> Bool
>     value::   ntype vtype ht lchildtype rchildtype -> vtype
>     reheight::(UNUM ht) => 
>               ntype vtype ht lchildtype rchildtype ->
>               ntype vtype BU lchildtype rchildtype
>     height:: ntype vtype ht lchildtype rchildtype -> ht
>     height _ = (undefined::ht)
> 
> -- A statically-typed leaf
> instance BBTree Leaf vtype Zero Nil Nil where
>     value (Leaf v h) = v
>     is_leaf = const True
>     reheight (Leaf v h) = (Leaf v (BU h))
> 
> -- A dynamically-typed leaf   
> instance BBTree Leaf vtype BU Nil Nil where
>     value (Leaf v h) = v
>     is_leaf = const True
>     height (Leaf v h) = h
> 
> -- A stanamically-typed node    
> instance BBTree Node vtype height lchildtype rchildtype where
>     is_leaf = const False
>     value (Node v h a b) = v
>     left (Node v h a b) = a
>     right (Node v h a b) = b
>     reheight (Node v h a b) = (Node v (BU h) a b)
>     height (Node v h a b) = h
    

Constructors:
    
> make_leaf:: vtype -> Leaf vtype Zero Nil Nil
> make_leaf v = Leaf v (undefined::Zero)
> 
> make_node:: (BBTree lnt lvt lh cl cr, BBTree rnt rvt rh cl' cr',
>              HeightC lh rh ph)
> 	     => vtype -> (lnt lvt lh cl cr) -> (rnt rvt rh cl' cr') ->
> 	     Node vtype ph (lnt lvt lh cl cr) (rnt rvt rh cl' cr')
> make_node v l r = let (c,h) = heightc (height l) (height r)
>                   in if c then Node v h l r else error "balance error"


Let's make our trees instances of a class Show, so we have something to show.

> instance (Show vtype, BBTree Leaf vtype height Nil Nil) => 
>          Show (Leaf vtype height Nil Nil)
>   where
>     show = show . value
>     
> instance (Show vtype, BBTree Node vtype height lchildtype rchildtype,
>           Show lchildtype, Show rchildtype)
>           =>
> 	  Show (Node vtype height lchildtype rchildtype)
>   where
>      show x = "[" ++ (show $ value x) ++ ": " ++ (show $ left x)
>                ++ "," ++ (show $ right x) ++ "]\n"
	      
Examples follow. They show off the true polymorphic nature of the trees.

> leaf1 = make_leaf 'a'
> leaf2 = make_leaf (1::Int)
> tree1 = make_node "b" leaf1 leaf2
> tree2 = make_node (Just 'a') tree1 leaf1

We can print out each of these trees by 'showing' them, or just by typing
leaf1, tree1, etc. at the GHCi prompt.

However, if we try the following,

*> tree3 = make_node False tree2 leaf1

we get a type error at compile time:

    No instance for (HeightC (Succ (Succ Zero)) Zero ph)
    arising from use of `make_node' at stanamically-balanced-trees.lhs:222
    In the definition of `tree3': make_node False tree2 leaf1

The error tells us of an attempt to build an unbalanced node, whose
children have heights two and zero.

As we mentioned above, static checks are sometimes too restrictive and
insufficient. For example, suppose we want to write a function that
builds a full binary tree. The first attempt could be as follows:

*> make_bbtree1 n = makeit n 0
*>    where
*>      makeit 0 counter = make_leaf counter
*>      makeit n counter = make_node counter (makeit n' c') (makeit n' (c'+1))
*>         where
*>    	 n' = n -1
*>    	 c' = 2*counter

Alas, it does not typecheck. First, we get an error stemming from a
monomorphic restriction. Even if we managed to get around that (as we
do in the following), we would still get an error message about an
infinite type. The compiler cannot tell, statically, that (makeit n'
c') and (makeit n' (c'+1)) construct trees of the same height. Indeed,
the height of the tree is a function of 'n', which is a run-time
value. Clearly, a run-type check is needed.

To defer static checks till the run-time, we need to enclose our types
into an envelope:

> data BW = forall ntype vtype lchildtype rchildtype .
>  	  (Show (ntype vtype BU lchildtype rchildtype), 
>            BBTree ntype vtype BU lchildtype rchildtype) => 
>  	 BW (ntype vtype BU lchildtype rchildtype)

Now we can write our function as follows:

> make_bbtree2 n = makeit n 0
>   where
>     makeit 0 counter = BW $ reheight $ make_leaf counter
>     makeit n counter = case (wlk,wrk) of
>                         (BW lk, BW rk) -> BW $ make_node counter lk rk
>       where
> 	 wlk = (makeit n' c')
> 	 wrk = (makeit n' (c'+1))
>   	 n' = n -1
>   	 c' = 2*counter

Now it types, and actually works:

> bwshow (BW t) = show t
> bwh (BW t) = height t

we can try "bwshow $ make_bbtree2 1" and "bwshow $ make_bbtree2 5"

We should point out that the invocation "make_node counter lk rk" has
exactly the same form as make_node in tree2 above. However, the latter
does a static check whereas the former checks the heights dynamically.

To see that the dynamic checking really works, suppose we wrote
make_bbtree as follows (with a small typo):

> make_bbtree3 n = makeit n 0
>   where
>     makeit 0 counter = BW $ reheight $ make_leaf counter
>     makeit n counter = case (wlk,wrk) of
>                         (BW lk, BW rk) -> BW $ make_node counter lk rk
>        where
> 	 wlk = (makeit n' c')
> 	 wrk = (makeit 0 (c'+1))
>   	 n' = n -1
>   	 c' = 2*counter
> 

If we try "bwshow$ make_bbtree3 3", we get a run-time exception alerting us
of a violation of the balancing condition.

-}