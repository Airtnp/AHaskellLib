-- Physical equality
-- ref: https://wiki.haskell.org/Physical_equality

{-

ect Equality for Lazy Languages
1.1 Why we need this
By Ahn, Ki Yung

Lazy languages like Haskell have a good excuse for not having object equality (or physical equality): the equality test is a hack for forcing evaluation. We can make use of object equality in Haskell if we can provide a resonable alternative for forcing evaluation; it would be even better if we can automatially decide when to evaluate. Therefore, we suggest we should search for a evaluation strategy that will dismiss the use of equality test as forcing evaluation.

Using equality test for forcing evaluation is against the philosophy of Haskell. Haskell has only one equality operator unlike Unlike Scheme or ML, which is to be simple and clean. But we have assigned strange operational semantics on equality. This is also problematic for partial evaluation; we cannot replace x==x with True.

The functional programming style of persistent data structures tend to generate shared objects frequently. Functional languages lacking object equality cannot have the full-benefit of sharing since they cannot optimize equality with object equality. Functional languages that cannot use object equality are giving up the optimization opportunity on the best-fitting domain. It is a common idiom to check object equality before structural equality. Object equality implies structural equality, unless we are dealing with non-deterministic objects. (There are rare exceptions such as OCaml nan, which is in my opinion a design flaw)

Therefore, our goal is to develop an evaluation strategy that intelligently forces evaluation when it is sure that forcing evaluation will not take too long and save space as well. For this we would need both static analysis and evaluation strategy at runtime. When we rely less on operational semantics of equality, we can have better optimization opportunities such as using object equality in equality tests.

1.2 Problems in optimizing equality
It is a common idiom to benefit from physical equality in C++.

A common idiom of C++ copy ctor, assignment and equality:

class C
{
...
   C(const C& c)
   {
       if (*this == &c) return; // the same object!! nothing to do
       // do the job if not the same object
   }

   C& operator = (const C& c)
   {
      if (*this == &c) return; // the same object!! nothing to do
      // do the job if not the same object
   }

   bool operator == (const C& c)
   {
      if (*this == &c) return true; // the same object!! nothing to do
      // do the job if not the same object
   }
...
};
However, haskell being a lazy langauge it has concerns that strict languages would not have. Let === be the physical equality. Even though === implies == except for bottom, there are some situations that we cannot by default optimize equality with physical equality. It is because haskell programmer use reflexive equality test as a common idom for deep evaluation.

1.2.1 Analyzing haskell insertion sort algorithm
These examples came up doing one of the homework in CS584 Algorithms (2006 Spring) class at Portland State University, lectured by Mark P. Jones. I choose haskell to implement sorting but memory leaked I added the counter. Thanks to Mark P. Jones for mentioning that equalities are used for forcing evaluation.

1.2.1.1 Insertion sort
    inssrt [] = []
    inssrt (x:xs) = ins x $ inssrt xs
      where ins z [] = [z]
            ins z l@(y:ys) | z <= y    = z : l
                           | otherwise = y : ins z ys
We test for the worst case when the length of the input is 1000 and it works fine.

Main> inssrt [1000,999..1]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
... ... ,988,989,990,991,992,993,994,995,996,997,998,999,1000]
1.2.1.2 Insertion sort with counter that leaks memory
    inssort p@(_,[]) = p
    inssort (n,x:xs) = ins x $ inssort (n,xs)
      where ins z (n,[]) = (n,[z])
            ins z (n,l@(y:ys))
              | z <= y    = (n+1, z:l)
              | otherwise = (n', y:ys') where (n',ys') = ins z (n+1,ys)
We test for the worst case when the length of the input is 1000 and it crashes because of memory leak due to constructing lengthy expression $1+1+1+\cdots $.

Main> inssort (0,[1000,999..1])
(Segmentation fault
1.2.1.3 Insertion sort with counter patched by obscure equality
    inssort' p@(_,[]) = p
    inssort' (n,x:xs) = ins x $ inssort' (n,xs)
      where ins z (n,[]) = (n,[z])
            ins z (n,l@(y:ys))
                | n==n && z <= y    = (n+1, z:l)
                | n==n && otherwise = (n', y:ys') where (n',ys') = ins z (n+1,ys)
We test for the worst case when the length of the input is 1000 and it works because of the equality check forces evaluation of the counter value.

Main> inssort' (0,[1000,999..1])
(499500,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
... ... ,986,987,988,989,990,991,992,993,994,995,996,997,998,999,1000])
1.3 Implementing physical equality in Haskell
I tried Hugs Version: 20050308 and GHC version 6.4.2 but only GHC worked. In Hugs, StablePtr a is not an instance of Eq.

implementation:
-}

import Foreign
import System.IO.Unsafe
 
x === y = unsafePerformIO $ do  
    px <- newStablePtr x    
    py <- newStablePtr y
    let ret = px == py
    freeStablePtr px
    freeStablePtr py
    return ret

{-
usage:

data S a = S a
 
s = S (S [1..])
s2 = S (S [1..])
s3 = s
   ___         ___ _
  / _ \ /\  /\/ __(_)
 / /_\// /_/ / /  | |      GHC Interactive, version 6.4.2, for Haskell 98.
/ /_\\/ __  / /___| |      http://www.haskell.org/ghc/
\____/\/ /_/\____/|_|      Type :? for help.

Loading package base-1.0 ... linking ... done.
Compiling Main             ( Tst.hs, interpreted )
Ok, modules loaded: Main.
*Main> s === s
True
*Main> s === s2
False
*Main> s === s3
True
This implementation works and concurrncy or garbage collection does not affect the result. But it is very slow since newStablePtr, freeStablePtr operations are expensive because it changes the state of the garbage collector. Cheap operation that just peeks the address is needed for the implementation. I think the best way is to provide a primitive operator for this.

1.4 Caveat
It should be noted that an equality operator like (===) above breaks referential transparency, so adding it to Haskell would ruin many of the nice properties that Haskell has and would make many program transformations invalid.

-}