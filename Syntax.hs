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