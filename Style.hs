-- Style
-- ref: https://wiki.haskell.org/Category:Style

-- Idioms/Avoiding

-- Idioms/Misc/Base cases and identites

-- Code duplication

-- Code duplication means that literal copies of a larger piece of code are present in a program. In all programming languages this is considered bad style, because it requires multiple maintenance of those pieces. Also a reader of the program might not immediately recognize the copies as duplicates, thus thinking there must be a subtle difference and trying to find it. Those differences could be small syntactic differences like semicolon vs. comma, or it can also be that the literally same code means something different in different contexts. Code duplication is either due to an undisciplined programmer or due to restricted expressiveness of the language.

-- In Haskell code duplication can also lead to unintended laziness breaks. E.g.

if b
  then 'a':x
  else 'a':y

-- contains the small code duplication of prepending 'a' to another list. The ugly effect is that if b is undefined, then the whole expression is evaluated to undefined. Put differently: As long as the evaluation of b has not finished, nothing from the expression can be evaluated.

-- This situation can be improved by

f b = 'a' : if b then x else y

-- f undefined !! 0 = 'a'

-- Commenting

-- It is generally a good idea to write comprehensible programs, and adding comments to your program can help to achieve that goal. However it is not true that every program can be become comprehensible by adding enough comments. In the first place you should write as clearly as possible such that comments are not necessary. Comments cannot be checked by the compiler and thus they tend to diverge from actual implementation. If you are going to write a comment, think about the following items before you actually write it:

-- Default method implementation

-- It's always a design question, whether to provide default implementions of methods of Type classes and how to design them.

{-

    Make only trivial default implementations.

        All instance declarations should be slim.

            Instance declarations (and default definitions) should never contain non-trivial function definitions.

            Instance declarations (and default definitions) should only provide "plumbing" to make existing functions accessible via the type class resolution mechanism.

            The "plumbed" functions should always be exported (since instances are always exported).


    Check that default implementations do not introduce type class dependencies that are not necessary otherwise

        E.g. if you have a Monad constraint, but no Functor constraint, then use liftM instead of fmap.

    If you have a choice of whether to implement the method default by custom code or by calling other methods of the same class, call methods of the same class. This reduces the amount of implementation work for class instances. It also makes it more probable, that no extra super-classes are needed.

        E.g. the default methods of the class providing divMod, div, and mod should implement the defaults, where divMod calls div and mod and vice versa.

    Do not try to implement mod by repeated subtraction or so.
    Document which methods must be implemented at least.

        E.g. "instances must implement divMod or (div and mod)".

    You may use a MINIMAL pragma so GHC can warn about incomplete instances.

        class Eq a where
            (==) :: a -> a -> Bool
            (/=) :: a -> a -> Bool
            x == y = not (x /= y)
            x /= y = not (x == y)
            { -# MINIMAL (==) | (/=) #- }

    Instance implementations should not call other methods of the same class with respect to instantiated type.


-}

-- Error reporting strategies
-- ref: http://blog.ezyang.com/2011/08/8-ways-to-report-errors-in-haskell-revisited/

-- error/Maybe a/Either String a/Monad/MonadError/throw in IO monad/IOError+catch/MonadTransformer/Checked exception/Failure/


-- Hierarchical module names

-- Hierarchical module names are an early extension of Haskell 98, now included in Haskell 2010. Haskell 98 only allowed atomic module names like List which was later moved into the hierarchical naming scheme as Data.List.

-- For new libraries the question arises, of where to place new modules. If you upload a package to Hackage it will check whether your modules at least use common names for module directories at the top level.

{-

Control
    Applicative
    Arrow
    Exception           -- (opt, inc. error & undefined)
    Concurrent          -- as hslibs/concurrent
        Chan            -- these could all be moved under Data
        MVar
        Merge
        QSem
        QSemN
        SampleVar
        Semaphore
    Parallel            -- as hslibs/concurrent/Parallel
        Strategies
    Monad               -- Haskell 98 Monad library
        ST              -- ST defaults to Strict variant?
            Strict      -- renaming for ST
            Lazy        -- renaming for LazyST
        State           -- defaults to Lazy
            Strict
            Lazy
        Error
        Identity
        Monoid
        Reader
        Writer
        Cont
        Fix              -- to be renamed to Rec?
        List
        RWS

Data
    Binary              -- Binary I/O
    Bits
    Bool                -- &&, ||, not, otherwise
    Tuple               -- fst, snd
    Char                -- H98
    Complex             -- H98
    Dynamic
    Either
    Int
    Maybe               -- H98
    List                -- H98
    PackedString
    Ratio               -- H98
    Word
    IORef
    STRef               -- Same as Data.STRef.Strict
        Strict          
        Lazy            -- The lazy version (for Control.Monad.ST.Lazy)
    Binary              -- Haskell binary I/O
    Digest
        MD5
        ...             -- others (CRC ?)
    Array               -- Haskell 98 Array library
        Unboxed
        IArray
        MArray
        IO              -- mutable arrays in the IO/ST monads
        ST
    Trees
        AVL
        RedBlack
        BTree
    Queue
        Bankers
        FIFO
    Collection
    Graph               -- start with GHC's DiGraph?
    FiniteMap
    Set
    Memo                -- (opt)
    Unique



-}

-- Import modules properly

-- Haskell has a lot of variants of importing identifiers from other modules. However not all of them are as comfortable as they seem to be at the first glance. We recommend to focus on the following two forms of import:

import qualified Very.Special.Module as VSM
import Another.Important.Module (printf, (<|>), )

-- instead of

import Very.Special.Module
import Another.Important.Module hiding (open, close, )

{-

    Style: If you read printf, <|> or VSM.open in the program you can find out easily where the identifier comes from. In the second case you don't know if these identifiers are from Very.Special.Module, Another.Important.Module or even other modules. Mind you that grep won't help, because Very.Special.Module and Another.Important.Module might just re-export other modules. You might guess the origin of printf according to its name, but for the infix operator <|> you will certainly have no idea.

    Compatibility: In the second case, if new identifiers are added to the imported modules they might clash with names of other modules. Thus updating imported modules may break your code. If you import a package A with version a.b.c.d that follows the Package versioning policy then within versions with the same a.b it is allowed to add identifiers. This means that if you import the suggested way, you can safely specify A >= a.b.c && <a.b+1 in your Cabal file. Otherwise you have to choose the smaller range A >= a.b.c && <a.b.c+1. It may also be that Another.Important.Module.open was deprecated when you hid it, and with a module update removing that identifier, your import fails. That is, an identifier that you never needed but only annoyed you, annoys you again, when it was meant to not bother you any longer! The first variant of import does not suffer from these problems.

    Correctness: I once found a bug in the StorableVector package by converting anonymous imports to explicit imports. I found out that the function Foreign.Ptr.plusPtr was imported, although functions from this module always have to calculate with unit "element" not "byte". That is, advancePtr must be used instead. Actually, the reverse function used plusPtr and this was wrong. A misbehaviour could only be observed for sub-vectors and elements with size greater than 1 byte. The test suite did miss that.

    Maintenance: All too often I find an old module that cannot be compiled any longer since it uses identifiers that do no longer exist. If the module imports implicitly and without qualification I have little chance to find out where the identifiers originally came from, what they meant and how they must be replaced.



-}

-- Since the Prelude is intended to be fixed for the future, it should be safe to use the hiding clause when importing Prelude.




-- Means of expression

-- Primary means of expression: variables, types, parentheses, modules, etc. in general all things that are relevant to the compiler

-- Secondary means of expression: language irrelevant layout and spacing, comments, conventions, etc. that is language elements that are irrelevant for the compiler, and thus are made exclusively for the human reader

-- Prefer primary means of expression to secondary ones!

-- Num instance for functions

instance Num b => Num (a -> b) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum

-- Then where functions can be added like Numbers, leaving their arguments open for later. The usefulness of this instance is debatable, and it is not currently part of the language report or defined in any popular library.
-- (sin^2 + cos^2) 123.4


-- Ord instance

-- An Ord instance may also suggest a notion of magnitude
-- An Ord instance may be free of any other association


-- Pairs of identifiers

-- Show and Read instance


-- Parameter order

-- The parameter order of Haskell functions is an important design decision when programming libraries. The parameter order shall
    -- allow piping

        -- Parameters in Haskell are rather reversed compared to imperative or object oriented languages. In an object oriented language, the object to work on is the very first parameter. In a function call it is often written even before the function name, say file in file.write("bla"). Strictly spoken, in Haskell it is not possible to alter objects, but there are many functions which return a somehow altered input object. This object should be the last parameter because then you can compose a sequence of operations on this object using the function composition operator (.).
 

    -- be consistent across similar functions

-- The more important the parameter, the more frequently it changes, the more it shall be moved to the end of the parameter list. If there is some recursion involved, probably the parameter, which you recurse on, is the one which should be at the last position.

-- If parameter b functionally depends on parameter a, then b should be before a.

-- Positive identifier

-- When choosing identifiers for Bool variables avoid negative namings!


-- Type signatures as good style

-- Using explicit type signatures is good style and GHC with option -Wall warns about missing signatures. Signatures are a good documentation and not all Haskell program readers have a type inference algorithm built-in. There are also some cases where the infered signature is too general for your purposes.

-- Why Haskell just works

-- 1 Types of errors

    -- Silly mistakes. These include things like typos, or just forgetting some minor thing here and there. Many of these would be caught by the type checker, or even the parser, though some might slip through.

    -- Unintentional mistakes. These are more serious mistakes than the silly mistakes, but they are still not due to a misunderstanding of the algorithm.

    -- Intentional mistakes. These are mistakes which are simply the result of not understanding the algorithm. The programmer really did intend for the program to do what it does, the fact that it is not correct is due to the programmer not understanding the algorithm.

-- 2 The method of computation

    {-

    There is an important difference in how computation is performed in imperative and functional languages. We shall first look at a strongly typed imperative language, and try to reason about how and where static checking might help to prevent errors, and where it won't.

    The fundamental operation of computation in an imperative language is the modification of state. An imperative program consists of an ordered sequence of state modifying commands. This is of course only true to some extent, since you can often use a functional style even in an imperative programming language, but in general the computations are performed primarily by modifying state. The important realisation here is that the result of a computation is not actually specified directly in an imperative programming language, but is extracted indirectly as a side effect of executing the state modifying commands. From this we see the result of an imperative computation depends on two things; the stateful commands, and the order in which these are executed. Of these two things only the former can be statically checked. Imperative languages in use today do very little meaningful static checking on the order of statements (and for good reason, it is very hard!). This is the key to why plenty of programs which compile just fine in a strongly typed imperative language, simply don't work properly at all; the static checking performed by the compiler can only catch a very small fraction of the errors that are possible.

    Functional programming is quite different. Here the fundamental operation of computation is function application. In a purely functional language like Haskell the question of "ordering" is simply not meaningful. The argument should now become apparent: Since the main operation of computation in functional languages (function application) is strongly typed, there's simply less room for errors to sneak in. So the difference, from a static checking standpoint, is that in an imperative language only part of the method of computation is actually checked, whereas for functional languages the entire method of computation can be type checked.

    Let's do an example. We'll write this in a fictional strongly typed imperative language, first using an imperative style, and then using a functional style. We don't use actual languages to better highlight the differences between the two versions. As a simple example we shall implement a part of a merge sort algorithm. We assume here that a function, merge, is available which will merge two sorted lists into one sorted list (for two unsorted list it will simply produce the wrong result).

    list.sort()

    {
    if ( self.size <= 1 ) 
    {
        return;
    }
    (list1,list2) = self.split( self.size / 2 );
    
    list1.sort();
    list2.sort();
    merged_list = merge( list1, list2 );
    self.set( merged_list );
    }
    For the sake of argument, let's assume that the programmer accidentally reordered the lines here, maybe she merges the lists before they are sorted, for example. A type 2 error. It's clear that such a mistake would mean the sorting routine won't produce the correct result, however it is also clear that the compiler won't be able to catch a mistake like this. Now, this is a fairly simple example, and naturally most people wouldn't make mistakes in simple examples like this, but hopefully you should be able to see how similar mistakes which are far less obvious and far easier to make are very common in imperative programming.

    The key idea, yet again, is that the result of the computation depends not only on the stateful operations themselves, but also the order in which they are executed. Only the former of these two aspects can actually be statically checked. It is clear that mistakes due to the order of operations can in general not be caught by the compiler, and this leads to many faulty programs that nonetheless compile happily.

    Let's rewrite the same thing in a more functional style. I should stress that this is not Haskell code, but the same made up pseudo language used above.

    list.sort() : list

    {
    if ( self.size <= 1 ) 
    {
        return self;
    }
    (list1,list2) = self.split( self.size / 2 );
    
    sorted_list1 = list1.sort();
    sorted_list2 = list2.sort();
    merged_list = merge( sorted_list1, sorted_list2 );
    return merged_list;
    }
    Notice that we only changed some minor things here. Most importantly the sort method no longer changes any state, it simply returns a new sorted list (rather than changing the current one in place).

    By switching to a functional approach we now see that the algorithm is expressed directly in the code, rather than indirectly as a product of stateful operations and their uncheckable ordering. If you were to make the same mistake in this code, trying to perform the merge before the sorts, you would get a compile time error. This is because the lists used in the merge, depends on the lists retrieved from the split, which depends on the list we're trying to sort. So you see that the "ordering" is made explicit and direct, and will be statically checked at every stage.

    You might claim the the equivalent error to the one used above would be to try to merge list1 and list2 rather than just moving the merge line up a bit, but that's not the case at all. We're talking about a type 2 error here. If the programmer understands the algorithm she will want to merge the sorted lists, not the unsorted lists. In the imperative version the names list1 and list2 both referred to the unsorted and sorted lists depending on where you referenced them, which is the whole root of the problem, whereas in the functional way of thinking only the names sorted_list1 and sorted_list2 refer to the sorted lists.

    -}

-- 3 Type of errors caught at compile time

    -- Silly mistakes. These errors are probably caught quite reliably by both imperative and functional programming languages, assuming they have a sane language design and strong typing. For this category of errors the property of being functional or imperative is probably less important than other properties.

    -- Unintentional mistakes. As we saw earlier, this type of mistake can quite often slide through the compiler's checks in an imperative language since for imperative code the important property of ordering is not checked. We also saw how this problem is helped by using a functional style together with static type checking. The fact that functional programming catches these errors while imperative programming does not, is probably largely the reason for why Haskell programs tend to "just work".

    -- Intentional mistakes. These are more serious errors, and are hardly ever caught in imperative languages. However, even these mistakes are often caught by functional programming languages for the same reason that errors of type 2 are. If you've misunderstood the algorithm, it is very likely that this will result in some type error when expressing it in a functional style, whereas if it is expressed in an imperative style the misunderstanding might take the form of an improper ordering of commands which cannot (in general) be caught. Haskell programmers will often testify that while working on a particularly complicated problem the type system helped them spot where their logic was flawed, and once the type errors were fixed so was their understanding.
