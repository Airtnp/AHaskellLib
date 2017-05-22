-- Super combinator
-- ref: https://wiki.haskell.org/Super_combinator
-- Constant applicative form(CAF)
-- ref: https://wiki.haskell.org/CAF
-- stackoverflow: http://stackoverflow.com/questions/8330756/what-are-super-combinators-and-constant-applicative-forms

-- Super combinator

-- A supercombinator is either a constant, or a combinator (no free variable) which contains only supercombinators as subexpressions.

-- Any lambda expression is of the form \x1 x2 .. xn -> E, where E is not a lambda abstraction and n≥0. (Note that if the expression is not a lambda abstraction, n=0.) This is a supercombinator if and only if:

-- the only free variables in E are x1..xn, and
-- every lambda abstraction in E is a supercombinator.    

-- Any lambda calculus expression (or, indeed, Haskell program) with no free variables can be converted into supercombinators using lambda lifting. For example, 

-- \f g -> f (\x -> g x 2) ---> \f g -> f ((\h x -> h x 2) g) 

-- Because supercombinators have no free variables, they can always be given their own names and let floated to the top level. 

-- \f g -> f ((\h x -> h x 2) g) --> let { scF h x = h x 2; scE f g = f (scF g) } in scE

-- A supercombinator which is not a lambda abstraction (i.e. for which n=0) is called a constant applicative form.

-- CAF

-- Any super combinator which is not a lambda abstraction. This includes truly constant expressions such as 12, ((+) 1 2), [1,2,3] as well as partially applied functions such as ((+) 4). Note that this last example is equivalent under eta abstraction to \ x  -> (+) 4 x which is not a CAF.

-- Since a CAF is a super combinator, it contains no free variables. Moreover, since it is not a lambda abstraction it contains no variables at all. It may however contain identifiers which refer to other CAFs, e.g.

a = c 3 where
    c = (*) 2

-- A CAF can always be lifted to the top level of the program. It can either be compiled to a piece of graph which will be shared by all uses or to some shared code which will overwrite itself with some graph the first time it is evaluated. A CAF such as

ints = from 1 where
    from n = n : from (n+1)

-- can grow without bound but may only be accessible from within the code of one or more functions. In order for the garbage collector to be able to reclaim such structures, we associate with each function a list of the CAFs to which it refers. When garbage collecting a reference to the function we collect the CAFs on its list.

-- It depends on how they desugar. `(4 +)` presumably desugars to `((+) 4)` and so is a CAF. If (+ 4) desugars to `\x -> x + 4` it isn't, if it desugars to `flip (+) 4` it is. -- DerekElkins

-- You've inverted it slightly (or it reads that way): it's reducibility that makes CAFs special. Since they are reducible they may imply work; work that could be saved. Nevertheless, yes, one typically doesn't care whether something's a CAF or not as far as meaning goes. Read the relevant parts of the book I linked to below, if you haven't already. As for ReferentialTransparency, whether or not something is viewed as a CAF doesn't change it's meaning, i.e. it is just a different implementation of the same function.

-- stackoverflow

-- Constant Applicative Forms, or CAFs for short, are top-level values defined in a program. Essentially, they are objects that are not allocated dynamically at run-time but, instead, are part of the static data of the program.