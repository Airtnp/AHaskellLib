-- Syntax
-- ref: https://wiki.haskell.org/Category:Syntax

-- Declaration vs. expression style

{-

Declaration style	                                
    Expression-style
where clause	                                    
    let expression
Function arguments on left hand side:	f x = x*x	
    Lambda abstraction:	f = \x -> x*x
Pattern matching in function definitions:	f [] = 0	
    case expression:	f xs = case xs of [] -> 0
Guards on function definitions:	f [x] | x>0 = 'a'	
    if expression:	f [x] = if x>0 then 'a' else ...

-}

-- Direction of data flow

-- That's just infixr/infixl

{-

In Haskell the direction of data flow symbolized by the notations differs amongst the notations. Both directions occur equally frequently:

from left to right:

function definition	 f x = x*x	(input left, output right)
Lambda	 \ x -> x*x
do notation	 do f; g
monadic composition	 f >>= g
let expression	 let x = 2 in x*x	(first definition, then usage)

from right to left:

function application	 f x,  f $ x	(input right, applied function left)
composition	 g . f
results of monads	 do x <- f
monadic composition	 g =<< f
where clause	 x*x where x = 2	(first usage, then definition)



-}

-- Do notation considered harmful

{-

Didactics

    -- The do notation hides functional details.
        -- Since do notation is used almost everywhere IO takes place, newcomers quickly believe that the do notation is necessary for doing IO,
        -- Newcomers might think that IO is somehow special and non-functional, in contrast to the advertisement for Haskell being purely functional,
        -- Newcomers might think that the order of statements determines the order of execution.
    -- The order of statements is also not the criterion for the evaluation order. Also here only the data dependencies count.



Library design

    -- Unfortunately, the do notation is so popular that people write more things with monads than necessary. See for instance the Binary package. It contains the Put monad, which in principle has nothing to do with a monad. All "put" operations have the monadic result (). In fact it is a Writer monad using the Builder type, and all you need is just the Builder monoid. Even more unfortunate, the applicative functors were introduced to Haskell's standard libraries only after monads and arrows, thus many types are instances of Monad and Arrow classes, but not as many are instances of Applicative. There is no special syntax for applicative functors because it is hardly necessary.


Safety

    -- The silent neglect of return values of functions. In an imperative language it is common to return an error code and provide the real work by side effects. In Haskell this cannot happen, because functions have no side effects. If you ignore the result of a Haskell function, the function will not even be evaluated.

    -- The situation is different for IO: While processing the IO, you might still ignore the contained return value.

Additional combinators

    -- Using the infix combinators for writing functions simplifies the addition of new combinators. Consider for instance a monad for random distributions. This monad cannot be an instance of MonadPlus, because there is no mzero (it would be an empty list of events, but their probabilities do not sum up to 1) and mplus is not associative because we have to normalize the sum of probabilities to 1. Thus we cannot use standard guard for this monad.

    However we would like to write the following:

    do f <- family
        guard (existsBoy f)
        return f
    Given a custom combinator which performs a filtering with subsequent normalization called 
    
    (>>=?) :: Distribution a -> (a -> Bool) -> Distribution a
    
    we can rewrite this easily:

    family >>=? existsBoy
    
    Note that the (>>=?) combinator introduces the risk of returning an invalid distribution (empty list of events),

Alternative combinators

    If you are used to writing monadic functions using infix combinators (>>) and (>>=) you can easily switch to a different set of combinators.

    This is useful when there is a monadic structure that does not fit into the current Monad type constructor class, where the monadic result type cannot be constrained.

    This is e.g. useful for the Set data type, where the element type must have a total order.

Useful

    Compare

        mdo x <- f x y z
            y <- g x y z
            z <- h x y z
            return (x+y+z)
    
    and

        mfix
        (\ ~( ~(x,y,z), _) ->
            do x <- f x y z
                y <- g x y z
                z <- h x y z
                return ((x,y,z),x+y+z))

-}


-- Formatting function types


hPutBuf :: Handle                       -- handle to write to
        -> Ptr a                        -- address of buffer
        -> Int                          -- number of bytes of data in buffer
        -> IO ()

-- If-Then-Else

-- Haskell without if-then-else syntax makes Haskell more logical and consistent. There is no longer confusion to beginners like: "What is so special about if-then-else, that it needs a separate syntax? I though it could be simply replaced by a function. Maybe there is some subtlety that I'm not able to see right now." There is no longer confusion with the interference of if-then-else syntax with do notation. Removing if-then-else simplifies every language tool, say compiler, text editor, analyzer and so on.

-- replace it by

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- Let vs. Where

-- Let: better 
f :: State s a
f = State $ \x ->
   let y = ... x ...
   in  y

-- Where: better

f x
  | cond1 x   = a
  | cond2 x   = g a
  | otherwise = f (h x a)
  where
    a = w x

-- vs. 

f x
  = let a = w x
    in case () of
        _ | cond1 x   -> a
          | cond2 x   -> g a
          | otherwise -> f (h x a)

f x =
   let a = w x
   in  select (f (h x a))
          [(cond1 x, a),
           (cond2 x, g a)]

f x
  = let a = w x
    in if cond1 x
       then a
       else if cond2 x
       then g a
       else f (h x a)

-- One other approach to consider is that let or where can often be implemented using lambda lifting and let floating, incurring at least the cost of introducing a new name. The above example:

-- where can hide the CAF, whereas let explicitly show them

-- In the second case, fib' is redefined for every argument x. The compiler cannot know whether you intended this -- while it increases time complexity it may reduce space complexity. Thus it will not float the definition out from under the binding of x.


-- In contrast, in the first function, fib' can be moved to the top level by the compiler. The where clause hid this structure and made the application to x look like a plain eta expansion, which it is not.


fib =
    let fib' 0 = 0
        fib' 1 = 1
        fib' n = fib (n - 1) + fib (n - 2)
    in  (map fib' [0 ..] !!)

fib x =
    let fib' 0 = 0
        fib' 1 = 1
        fib' n = fib (n - 1) + fib (n - 2)
    in  map fib' [0 ..] !! x

-- List comprehension

-- No export lists

module Important (
   {- * Important functions -}
   foo,
 
   {- * Important data types -}
   Number(One,Two,Three)) where
 
 
{- | most important function -}
foo :: Int
foo = 2
 
{- | most important data type -}
data Number =
     Zero
   | One
   | Two
   | Three
   | Many

-- Pattern guard

lookup :: FiniteMap -> Int -> Maybe Int
 
addLookup env var1 var2
   | Just val1 <- lookup env var1
   , Just val2 <- lookup env var2
   = val1 + val2
{-...other equations...-}

-- will check to see if both lookups succeed, and bind the results to val1 and val2 before proceeding to use the equation.

-- Pronunciation

-- infix
-- partial: section


-- Syntax Suger
-- cons: https://wiki.haskell.org/Syntactic_sugar/Cons
-- pros: https://wiki.haskell.org/Syntactic_sugar/Pros
-- ref: https://wiki.haskell.org/ThingsToAvoid/Discussion

-- Terminator vs. separator

-- Terminator: There is one symbol after each element.
-- Separator: There is a symbol between each element. This is what the functions Data.List.intersperse and Data.List.unwords generate. In Haskell language, the following syntaxes allow separators only:

{-

Liberal choice between separators and terminators:

export lists: module A(a,b,c) where and module A(a,b,c,) where (and module A(a,b,c,,,) where ...)
import lists: import A(a,b,c) and import A(a,b,c,)
let syntax: let a = 'a'; b = 'b' in ... and let a = 'a'; b = 'b'; in ...
do syntax: do a ; b and do a; b;

-}

-- Unary operator

-- In Haskell there is only one unary operator, namely the unary minus. It has been discussed in length, whether the unary minus shall be part of numeric literals or whether it shall be an independent operator.

-- View pattern 
-- ref: https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns

type Typ

data TypView = Unit
            | Arrow Typ Typ

view :: Typ -> TypView

-- additional operations for constructing Typ's ...

-- In current Haskell, using this signature is a little inconvenient: It is necessary to iterate the case, rather than using an equational function definition. And the situation is even worse when the matching against t is buried deep inside another pattern. In response, programmers sometimes eschew type abstraction in favor of revealing a concrete datatype that is easy to pattern-match against.

size :: Typ -> Integer
size t = case view t of
    Unit -> 1
    Arrow t1 t2 -> size t1 + size t2

-- View patterns permit calling the view function inside the pattern and matching against the result:

size (view -> Unit) = 1
size (view -> Arrow t1 t2) = size t1 + size t2

{-

Scoping for expr -> pat:

The variables bound by the view pattern are the variables bound by pat.
Any variables in expr are bound occurrences. Variables bound by patterns to the left of a view pattern expression are in scope. For example:
In function definitions, variables bound by matching earlier curried arguments may be used in view pattern expressions in later arguments.
   example :: (String -> Integer) -> String -> Bool
   example f (f -> 4) = True
Variables can be bound to the left in tuples and data constructors:
   example :: ((String -> Integer,Integer), String) -> Bool
   example ((f,_), f -> 4) = True
Typing If expr has type t1 -> t2 and pat matches a t2, then the whole view pattern has type t1.

Evaluation To match a value v against a pattern (expr -> pat), evaluate (expr v) and match the result against pat.

-}

-- Both pattern

-- A "both pattern" pat1 & pat2 matches a value against both pat1 and pat2 and succeeds only when they both succeed. A special case is as-patterns, x@p, where the first pattern is a variable. Both patterns can be programmed using view patterns:

both : a -> (a,a)
both x = (x,x)

f (both -> (xs, h : t)) = h : (xs ++ t)

-- As-pattern

-- x@p

-- N+k patterns

np :: Num a => a -> a -> Maybe a
np k n | k <= n = Just (n-k)
       | otherwise = Nothing

fib :: Num a => a -> a
fib 0 = 1
fib 1 = 1
fib (np 2 -> Just n) = fib (n + 1) + fib n