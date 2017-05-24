-- Non-strict semantics
-- ref: https://wiki.haskell.org/Non-strict_semantics

-- An expression language is said to have non-strict semantics if expressions can have a value even if some of their subexpressions do not. Haskell is one of the few modern languages to have non-strict semantics by default: nearly every other language has strict semantics, in which if any subexpression fails to have a value, the whole expression fails with it.

-- This is one of the most important features in Haskell: it is what allows programs to work with conceptually infinite data structures, and it is why people say that Haskell lets you write your own control structures. It's also one of the motivations behind Haskell being a pure language (though there are several other good ones).

-- 1. What

-- Any sufficiently capable programming language is non-total, which is to say you can write expressions that do not produce a value: common examples are an exception thrown, an infinite loop, or unproductive recursion, e.g. the following definition in Haskell:

-- bottom
noreturn :: Integer -> Integer
noreturn x = negate (noreturn x)

-- [2, 4, noreturn (5)]
-- In python, this will failed. his is called innermost-first evaluation: in order to call a function with some arguments, you first have to calculate what all the arguments are, starting from the innermost function call and working outwards. The result is that Python is strict, in the sense that calling any function with an undefined argument produces an undefined value, i.e. f(⊥) = ⊥. If your language uses innermost-first evaluation, it correspondingly must have strict semantics.

-- In Haskell, The program does not have to compute noreturn 5 because it is irrelevant to the overall value of the computation: only the values that are necessary to the result need be computed. This is called outermost-first evaluation because you first look at the outermost function call, elem, to see if it needs to use its arguments, and only if it does do you look at what those arguments are. This means that you can write a function that doesn't look at its argument, so it will return a value even if the argument is ⊥. Such functions are not strict, i.e. they satisfy f(⊥) ≠ ⊥. Practically, this means that Haskell functions need not completely compute their arguments before using them, which is why e.g. take 3 [1..] can produce [1,2,3] even though it is given a conceptually infinite list.

-- Note that outermost-first evaluation is not the only way to have non-strict semantics: a speculative evaluation strategy, that evaluates arguments in parallel with the function in case they are needed later, could also be non-strict, as long as whenever the speculative evaluation failed, the evaluation of the function continued.

-- Note also that in order for a function to be truly non-strict, it must return something without inspecting its argument at all. You might think that doesn't sound like a very useful function, but remember that it might be e.g. a partial application: the function (||) True, or equivalently \x -> True || x does not need to inspect its argument, since True || x is always True. There are other examples, too: constructors like Just wrap their argument without inspecting it, and some other functions apply constructors before looking at the argument, and hence still produce a partial result, e.g. inits ⊥ = [] : ⊥


-- 2. Why

-- Note that outermost-first evaluation is not the only way to have non-strict semantics: a speculative evaluation strategy, that evaluates arguments in parallel with the function in case they are needed later, could also be non-strict, as long as whenever the speculative evaluation failed, the evaluation of the function continued.

-- Note also that in order for a function to be truly non-strict, it must return something without inspecting its argument at all. You might think that doesn't sound like a very useful function, but remember that it might be e.g. a partial application: the function (||) True, or equivalently \x -> True || x does not need to inspect its argument, since True || x is always True. There are other examples, too: constructors like Just wrap their argument without inspecting it, and some other functions apply constructors before looking at the argument, and hence still produce a partial result, e.g. inits ⊥ = [] : ⊥

-- But in a non-strict context, even if both or and map are written completely naïvely, when or gets to the first True it stops asking for any more booleans, so map doesn't need to produce any more of them, and none of the rest of the list is visited.

-- 3. But that's so weird

-- Not really! In non-strict languages you typically have evaluation driven by need, whereas in strict languages you have evaluation driven by function application. But functions are already for abstraction, so they end up serving a sort of dual purpose; meanwhile ordinary values can't really be used for abstraction, except if you know you're going to use their value at least once. If you don't, you have to wrap your value in a function that doesn't take any arguments, or in certain type systems where that doesn't make sense as a concept, you have to use a function that takes a single, boring argument, that it then ignores. You then have to duplicate the work if you want to use it twice, or else write some sort of caching, probably using mutable variables. On top of all that, you decide that function application isn't even the only method of driving evaluation, because you also need if-statements, loops, and other control structures that you have to bake right into the fabric of your language.

-- 4. How do I stop it?

-- As mentioned above, non-strictness can hurt performance, e.g. if a result is definitely going to be needed later, you might as well evaluate it now, to avoid having to hold on to all the data that goes into it. Fortunately, the Haskell designers were aware of these problems and introduced a loophole or two so that we could force our programs to be strict when necessary: see Performance/Strictness and seq.