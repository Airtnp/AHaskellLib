-- Combinators
-- ref: https://wiki.haskell.org/Combinator
-- ref: https://wiki.haskell.org/Combinator_pattern
-- ref: https://wiki.haskell.org/Combinatory_logic
-- ref: https://wiki.haskell.org/Super_combinator

-- There are two distinct meanings of the word "combinator" in common usage.

-- The first is a narrow, technical meaning, namely: A function or definition with no free variables. A "function with no free variables" is a pure lambda-expression that refers only to its arguments, like

\a -> a
\a -> \b -> a
\f -> \a -> \b -> f b a

-- and so on. The study of such things is called combinatory logic. They are certainly used in Haskell -- the examples above are id, const, and flip respectively. Many of the functions involved in the Applicative instance for ((->) e) also fall into this category. But such examples are fairly limited.

-- The second meaning of "combinator" is a more informal sense referring to the combinator pattern, a style of organizing libraries centered around the idea of combining things. This is the meaning of "combinator" which is more frequently encountered in the Haskell community. Usually there is some type T, some functions for constructing "primitive" values of type T, and some "combinators" which can combine values of type T in various ways to build up more complex values of type T.


-- Combinator pattern

-- Libraries such as Parsec use the combinator pattern, where complex structures are built by defining a small set of very simple 'primitives', and a set of 'combinators' for combining them into more complicated structures. It's somewhat similar to the Composition pattern found in object-oriented programming.

-- Super combinators

-- A supercombinator is either a constant, or a combinator which contains only supercombinators as subexpressions.

-- Any lambda expression is of the form \x1 x2 .. xn -> E, where E is not a lambda abstraction and n≥0. (Note that if the expression is not a lambda abstraction, n=0.) This is a supercombinator if and only if:

    -- the only free variables in E are x1..xn, and
    -- every lambda abstraction in E is a supercombinator.

-- \f g -> f (\x -> g x 2)
-- combinator, not supercombinator
-- \f g -> f ((\h x -> h x 2) g)
-- Then it's supercombinator by lambda lifting