-- Polyvariadic functions
-- ref: https://wiki.haskell.org/Polyvariadic_functions
-- oleg: http://okmij.org/ftp/Haskell/polyvariadic.html#polyvar-fn

-- It is sometimes claimed that Haskell does not have polyvariadic functions. Here we demonstrate how to define functions with indefinitely many arguments; those arguments do not have to be of the same type.

-- The code referenced below shows that defining polyvariadic functions takes only a few lines of Haskell code, and requires only the most common extension of multiparameter classes with functional dependencies. Here is an example of using a polyvariadic function, build, which takes an arbitrary number of arguments and returns them in a list. The function build is first class, and so can be passed as an argument to other functions, such as use_build below.

use_build::(forall r a. (BuildList a r) => a -> r)
        -> x -> x -> x -> x -> [[x]]
use_build bb a b c d =
    let t1 = bb a                -- bb is indeed polyvariadic
        t2 = bb a b
        t3 = bb a b c
        t4 = bb a b c d
        t5 = bb a b c d a
    in [t1,t2,t3,t4,t5]
test_ub = use_build build 'a' 'b' 'c' 'd'
-- result: ["a","ab","abc","abcd","abcda"]

{-

  Functions with the variable number (of variously typed) arguments

	* Motivation
	* Implementation
	* Vararg functions are first-class
	* Variable number of variably typed arguments
	* Variable number of really variably typed arguments
	* How does that work?
	* The function build and polymorphic Num constants


* Motivation

In a message 'how to write a list builder? fixpoint?'  posted on the
Haskell mailing list on June 1, 2004, Ben Yu wrote:

 ] Is it possible to write a function to build a list [a]?
 ] so that I can write [a,b,c,d] as "getBuilt $ build a b c d"?

Essentially this is the question about the possibility of polyvariadic
functions in Haskell. At first blush, the answer seems
negative. Indeed, footnote 2 of the article "The Web" by Ralf Hinze
and Johan Jeuring (JFP, The Functional Pearl) says: "Since Haskell
currently has no support for defining variadic functions, the Web
library only supplies con0, ..., con_max where max is some fixed upper
bound." This message shows that Haskell with the commonest extension
(multi-parameter type classes and functional dependencies) has
supported functions with the indefinite number of arguments all along.

Furthermore, the implementation takes only few lines.


* Implementation

> \{-# OPTIONS -fglasgow-exts #-\}
>
> module VarArg where
> import Data.FiniteMap   -- for an example below
>
> class BuildList a r  | r-> a where
>     build' :: [a] -> a -> r
>
> instance BuildList a [a] where
>     build' l x = reverse$ x:l
>
> instance BuildList a r => BuildList a (a->r) where
>     build' l x y = build'(x:l) y
>
> --build :: forall r a. (BuildList a r) => a -> r
> build x = build' [] x

That's it. It works both in GHC and Hugs.

*VarArg> build True :: [Bool]
[True]

*VarArg> build True False :: [Bool]
[True,False]

*VarArg> build True False False ++ build True False False
[True,False,False,True,False,False]

*VarArg> build 'a' 'b' 'c' 'd' 'e' :: [Char]
"abcde"

*VarArg> build (1::Int) (2::Int) :: [Int]
[1,2]

*VarArg> build (1::Int) (2::Int) (3::Int) :: [Int]
[1,2,3]

Note that the type annotation [Bool] or a similar hint at the end is
required: it is the delimiter of the list. Who would have thought that
the type annotation can play the role of Nil...

We must also note that for the function build', it is the type of its
continuation that determines the type of its argument.

* Vararg functions are first-class

Polyvariadic functions are first-class and can be passed to other
functions as arguments. We should keep in mind however that
polyvariadic functions are actually class methods and are therefore
polymorphic. A function that takes a polymorphic function and uses it
polymorphically has a higher-ranked type. Higher-ranked types cannot
be inferred (in general) and must be declared explicitly. In great
detail, this issue is discussed in Ken Shan's survey ``Sexy types
in action,'' ACM SIGPLAN Notices 39(5):15-22, 2004:
	http://www.eecs.harvard.edu/~ccshan/cs252/usage.pdf

The following code illustrates passing of the above 'build' function
as an argument to other functions. We can use the latter argument
polyvariadically.

> -- Higher-ranked type: the signature is required
> use_build::(forall r a. (BuildList a r) => a -> r) 
>            -> x -> x -> x -> x -> [[x]]
> use_build bb a b c d =
>   let t1 = bb a
>       t2 = bb a b
>       t3 = bb a b c
>       t4 = bb a b c d
>       t5 = bb a b c d a
>  in [t1,t2,t3,t4,t5]
>  
> test_ub = use_build build 'a' 'b' 'c' 'd'
>
> -- *VarArg> test_ub
> -- ["a","ab","abc","abcd","abcda"]

The following example demonstrates another aspect of first-classness
of 'build' -- storing it in a data structure. The code also requires
fewer signatures. In the example, 'build' builds itself. The function
really has quite a few faces...

> data W = W (forall r a. (BuildList a r) => (a->r))
>
> test2 = let t1 = build (W build)
>             t2 = build (W build) (W build)
> 	      t3 = t1 ++ t2
>	      f (W b) = b (1::Int) ++
> 	                b (1::Int) (2::Int) ++
>		        b (1::Int) (2::Int) (3::Int)
>	  in map f t3
>

*VarArg> test2
[[1,1,2,1,2,3],[1,1,2,1,2,3],[1,1,2,1,2,3]]


* Variable number of variably typed arguments

The following simple variation defines a polyvariadic function to
construct associative lists or finite maps:

> class Builder2 c a b r | r -> a where
>   build2 :: (a->b->c a b->c a b) -> c a b -> a -> b -> r
> instance Builder2 c k v (c k v) where
>   build2 acc seed k v = acc k v seed
> instance Builder2 c a b r => Builder2 c a b (a->b->r) where
>   build2 acc seed a b x y = build2 acc (acc a b seed) x y
>
> newtype AL a b = AL [(a,b)] deriving Show
>
> test_bb1::AL String Bool
> test_bb1 = build2 (\x y (AL l) -> AL $ (x,y):l) (AL []) "a" True "b" False
>
> test_bb2:: FiniteMap String Bool
> test_bb2 = build2 (\x y m -> addToFM m x y) emptyFM 
>            "a" True "b" False "c" True

The function build2 not only has the variable number of
arguments. These arguments don't even have to be of the same type.


* Variable number of really variably typed arguments

Functions with the variable number of variably typed arguments are
especially useful for creating strongly-typed heterogeneous
collections such as lists and records:
	http://homepages.cwi.nl/~ralf/HList/

The following is a snippet from the source code accompanying the HList
paper (Haskell Workshop, 2004):

HList> let x = hBuild True in hEnd x
HCons True HNil

HList> let x = hBuild True 'a' in hEnd x
HCons True (HCons 'a' HNil)

HList> let x = hBuild True 'a' "ok" in hEnd x
HCons True (HCons 'a' (HCons "ok" HNil))

HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5))
HCons (Key 42) (HCons (Name "Angus") (HCons Cow (HCons (Price 75.5) HNil)))

HList> hEnd (hBuild (Key 42) (Name "Angus") Cow (Price 75.5)) == angus
True

Here we used a trivial function hEnd for list termination:
    hEnd t@(HCons x y) = t

Note: 
  - x :: HCons a b
       means: forall a b. x :: HCons a b
  - hEnd x
       means: exists a b. x :: HCons a b



* How does that work?

So the key here appears to be that the type-checker can distinguish 
between function application and other uses of an expression?

Indeed, the trick is straightforward -- merely basic typechecking.
Let us recall the definition of build'

*> class BuildList a r  | r-> a where
*>     build' :: [a] -> a -> r
*>
*> instance BuildList a [a] where
*>     build' l x = reverse$ x:l
*>
*> instance BuildList a r => BuildList a (a->r) where
*>     build' l x y = build'(x:l) y

Let us consider the expression

	build' [] 'a' 'b' ++ []

or, in a more abstract syntax

	(++) ((build' [] 'a') 'b') []

We make use of the familiar typing rule

G |- e1: t1->t2    G |- e2: t1  <===> G |- (e1 e2) : t2

where G is the typing environment (which associates types with the variables
that appear free in expressions e1 and e2). The typing rule can be applied
in either direction.

Operator (++) has the type [t1] -> [t1] -> [t1]. Given that fact and
applying the typing rule twice, we infer that ((build' [] 'a') 'b')
has the type [t1], [] has the type [t1], and so the whole expression
has the type [t1]. We have inferred that
	((build' [] 'a') 'b') :: [t1]
Applying the rule in reverse direction, we find that
	(build' [] 'a'):: t3->[t1]   'b':: t3
Or, because 'b' has the type Char, t3 unifies with Char, which gives
us
	(build' [] 'a') :: Char->[t1]

Applying the typing rule several more times, we find
	[]:: t4
	'a' :: t5
	build':: t4 -> t5 -> (Char->[t1])
or
	[]::t4
	build' :: t4 -> Char -> (Char->[t1])

From the declaration of the class BuildList, we know that 
	build' :: [a] -> a -> r
By unifying the two signatures, we find
	build' :: [Char] -> Char -> (Char->[t1])

Of the two instances of BuildList, the second obviously matches.
The functional dependency r->a essentially tells us that 'r' is the
primary key in the instance selection. That is, if we use (Char->[t1])
to match with the second parameter of the instance and found the
matching instance, we can be sure that it is the only matching instance
-- no matter how many instances are added in the future or contained
in other modules. 
So, we commit to the second instance and 
assign the type to build' in the original expression as
	build'_2 :: [Char] -> Char -> (Char->[t1])

However, the second instance of BuildList includes the term
"build' (x:l) y", which we have to type in turn. We know that
	(x:l)::[Char]
	(build' (x:l) y)::[t1]
from which we determine, with the same typing rule,
	build'::[Char] -> t6 -> [t1]
or, unifying with the signature for build',
	build'::[Char] -> Char -> [t1]
Again, we search for the appropriate instance of BuildList. Now, the
first instance matches. Again, we commit to it. Unifying 
'a' with Char and [t1] with [a] gives us
	build'_1 :: [Char] -> Char -> [Char]

and the type of the whole (++) expression as [Char]. Typechecking succeeds,
and we were able to unambiguously choose instances.

The details of the actual typechecking algorithm most likely differ,
yet the general idea -- and the outcome -- are the same.


* The function build and polymorphic Num constants

Why just "build 1 2 3" or "build' [] 1 2 3" don't work?

Well,

*VarArg> build 1 ++ []
[1]
*VarArg> :t build 1 ++ []
build 1 ++ [] :: forall a. (Num a) => [a]

work as expected. Just "build 1" per se cannot work because
the typechecker selects the instance of BuildList based on the return
type. In the case of a mere "build 1", the return type is
indeterminate: it could be a function or could be a list. The
typechecker needs either the explicit type specification, e.g.,
::[Int], or at least a hint (which " ++ []" above provides).

If we specify more than one polymorphic constant, we have to give more
precise hints:

*VarArg> build (1::Int) (2::Int) ++ []

Or, if we wish to keep the constants (and hence the result)
polymorphic:

*VarArg> build (1::Num a =>a) (2::Num a =>a) ++ []
[1,2]
*VarArg> build (1::Num a =>a) (2::Num b =>b) (3::Num c=>c)++ []
[1,2,3]

One may question the need for those annotations: if we can say
[1,2,3], which types to Num a => [a], why can't we say "build 1 2 3"
and have the inferred type to be likewise Num a => [a]?

Here [1,2,3] is a syntactic sugar for 1:(2:(3:[])). The operator (:) has
the type
	(:) :: forall a. a -> [a] -> [a]
Since (:) is actually a function, the type of the result (which is
[a]->[a]) is unambiguously determined by the type of the argument,
'a'. Informally, the type of a function has a functional dependency. 

The function build' is a member of a class such that the type of the
_result_ of build' unambiguously determines the type of the
argument. Note the reverse implication. Therefore, once we specify
the type of the result, everything works out. And it does:

VarArg> build 1 2 3 4 :: [Int]
[1,2,3,4]

In Hugs. But not in GHC (6.0.1). My impression is that GHC assumes
overlapping instances when choosing instances -- even if no
-fallow-overlapping-instances flag is present. In order to get the
example work in GHC, we have to assure that all arguments to build
have the same type in some other way, e.g., using local type
variables:

*VarArg> let (a::t)=1 in let b=(2::t); c=(3::t) in build a b c a b c ::[t]
[1,2,3,1,2,3]

The result is actually polymorphic, Num t => [t].

One may say that giving each polymorphic argument of build an explicit
signature is a lot of keystrokes. We should keep in mind however that
these problems typically arise only when we enter expressions
involving polymorphic functions at GHCi's prompt. If we use the same
expressions in real code, very often enough type information is
available for the typechecker to successfully resolve overloading. For
example, people often give signatures to top level functions (for
better documentation, for explicitly stating the function's contract
and other social reasons). These signatures also help the typechecker.



-}