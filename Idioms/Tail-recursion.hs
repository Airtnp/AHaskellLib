-- Tail recursion
-- ref: https://wiki.haskell.org/Tail_recursion

-- A recursive function is tail recursive if the final result of the recursive call is the final result of the function itself. If the result of the recursive call must be further processed (say, by adding 1 to it, or consing another element onto the beginning of it), it is not tail recursive.

{-

Here is formal definition of "tail recursive". "f occurs in t" means f is a free variable of t.

When a function is defined (in let or at the top level) as:

 f = t

where f is a name and t is a lambda-term, f is tail recursive iff f occurs tail recursively in t. f occurs tail recursively in t iff f occurs in t and any of the following holds:

    t is variable;
    
    t is "\var -> t0" and f occurs tail recursively in t0;
    
    t is "t0 t1" and f occurs tail recursively in t0 and does not occur in t1;
    
    t is "let bs in t0" and f occurs tail recursively in t0 and for each binder "var = t1" in bs, f does not occur in t1;
    
    t is "case t0 of bs" and f does not occur in t0 and for each branch b in bs, f does not occur or occurs tail recursively in b;
        
        when we are saying "occur in b", b has form "D vars -> t" (where D is some data constructor and vars is a sequence of names), we are thinking of the lambda-abstraction "\vars -> t" instead of b.



-}]

-- Note that foldl is tail recursive.

-- The important concept to know in Haskell is guarded recursion (see tail recursion modulo cons), where any recursive calls occur within a data constructor (such as foldr, where the recursive call to foldr occurs as an argument to (:)). This allows the result of the function to be consumed lazily, since it can be evaluated up to the data constructor and the recursive call delayed until needed.

-- TCO (Tail call optimisation)

-- In many programming languages, calling a function uses stack space, so a function that is tail recursive can build up a large stack of calls to itself, which wastes memory. Since in a tail call, the containing function is about to return, its environment can actually be discarded and the recursive call can be entered without creating a new stack frame. This trick is called tail call elimination or tail call optimisation and allows tail-recursive functions to recur indefinitely.

-- In Haskell, the function call model is a little different, function calls might not use a new stack frame, so making a function tail-recursive typically isn't as big a deal—being productive, via guarded recursion, is more usually a concern.

