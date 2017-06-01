-- Untypechecking
-- ref: https://wiki.haskell.org/Untypechecking

-- Retify
-- Converting from a type to a term.

{-

[Haskell] De-typechecker: converting from a type to a term

oleg at pobox.com oleg at pobox.com
Tue Mar 1 03:13:08 EST 2005

  * Next message: [Haskell] Re: Type of y f = f . f
  * Messages sorted by: [ date ] [ thread ] [ subject ] [ author ]

-----------------------------------------------------------------------------------------------------

This message presents polymorphic functions that derive a term for a
given type -- for a class of fully polymorphic functions: proper and
improper combinators. This is better understood on an example:

  rtest4 f g = rr (undefined::(b -> c) -> (a -> b) -> a -> c) HNil f g
  *HC> rtest4 (:[]) Just 'x'
  [Just 'x']
  *HC> rtest4 Just Right True
  Just (Right True)

We ask the Haskell typechecker to derive us a function of the
specified type. We get the real function, which we can then apply to
various arguments. The return result does behave like a `composition'
-- which is what the type specifies. Informally, we converted from
`undefined' to defined.

It must be emphasized that no modifications to the Haskell compiler are
needed, and no external programs are relied upon. In particular,
however surprising it may seem, we get by without `eval' -- because
Haskell has reflexive facilities already built-in.

This message contains the complete code. It can be loaded as it is
into GHCi -- tested on GHCi 6.2.1 or GHCi snapshot 6.3.20041106. It
may take a moment or two to load though. Commenting our tests speeds
up the loading.


This message presents two different converters from a type to a term.
Both derive a program, a term, from its specification, a type -- for
a class of fully polymorphic functions. The first converter has just
been demonstrated.  It is quite limited in that the derived function
must be used `polymorphically' -- distinct type variables must be
instantiated to different types (or, the user should first
instantiate their types and then derive the term). The second
converter is far more useful: it can let us `visualize' what a
function with a particular type may be doing.  For example, it might
not be immediately clear what the function of the type
    (((a -> b -> c) -> (a -> b) -> a -> c) ->
     (t3 -> t1 -> t2 -> t3) -> t) -> t)

is. The test (which is further down) says

    test9 = reify (undefined::(((a -> b -> c) -> (a -> b) -> a -> c) ->
                           (t3 -> t1 -> t2 -> t3) -> t) -> t) gamma0

    *HC> test9
    \y -> y (\d h p -> d p (h p)) (\d h p -> d)

that is, the function in question is one of the X combinators. It is
an improper combinator.

A particular application is converting a point-free function into the
pointful form to really understand the former. For example, it might
take time to comprehend the following expression

 pz = (((. head . uncurry zip . splitAt 1 . repeat) . uncurry) .) . (.) . flip

It is derived by Stephan Hohe in
  http://www.haskell.org/pipermail/haskell-cafe/2005-February/009146.html

Our system says
  test_pz = reify (undefined `asTypeOf` pz) gamma0
  *HC> test_pz
  \h p y -> h y (p y)

So, pz is just the S combinator.

As an example below shows, an attempt to derive a term for a type
a->b expectedly fails. The type error message essentially says that
a |- b is underivable.


Our system solves type habitation for a class of functions with
polymorphic types. From another point of view, the system is a prover
in the implication fragment of intuitionistic logic. Essentially we
turn a _type_ into a logical program -- a set of Horn clauses -- which
we then solve by SLD resolution.  It is gratifying to see that
Haskell typeclasses are up to that task.


The examples above exhibit fully polymorphic types -- those with
*uninstantiated* -- implicitly universally quantified -- type
variables. That is, our typeclasses can reify not only types but also
type schemas. The ability to operate on and compare *unground* types
with *uninstantiated* type variables is often sought but rarely
attained. The contribution of this message is the set of primitives
for nominal equality comparison and deconstruction of unground
types. Earlier these tools were used to implement nested monadic
regions in the type-safe manner without imposing the total order on
the regions. The monadic regions are described in the paper by M. Fluet
and G. Morrisett, ICFP04.

The rest of this message is as follows:

        - equality predicate of type schemas
        - function types as logic programs. Solving type habitation
          by SLD resolution
        - more examples
        - code
           -- class Resolve: SLD resolution

* Equality predicate of type schemas

This is a short remark on the equality predicate of type schemas. In
more detail, this topic will be described elsewhere.

To obtain the equality, we need the following overlapping instances
extensions

> \{-# OPTIONS -fglasgow-exts #-\}
> \{-# OPTIONS -fallow-undecidable-instances #-\}
> \{-# OPTIONS -fallow-overlapping-instances #-\}
> \{-# OPTIONS -fallow-incoherent-instances #-\}
>
> module HC where


We must remark that -fallow-overlapping-instances and
-fallow-incoherent-instances extensions are used *exclusively* for the
type equality testing. With these extensions and the following declarations

  class C a where op:: a -> Int
  instance C a where ...
  instance C Bool where ...
  data W  = forall a. W a

the expression
  case W () of W x -> op x

is well-typed with the "instance C a" chosen. In the context of TypeEq,
that behavior guarantees that a quantified variable is equal only to itself
and nothing else. So, the use of overlapping instances in this code
is well-defined and sound.

The existence of this remarkable feature has been pointed out by Simon
Peyton-Jones, who implemented it.

The code at the end of this message defines nominal equality on
quantified type variables. A quantified variable is equal only to
itself. It is useful to think of such type variables as being
Skolemized. Fortunately, GHC kindly does all the Skolemization for
us. For example,

        *FP> type'eq id id
        HFalse

because the type of the identity function "a->a" means "forall a. a
->a", which, after skolemization, becomes "sk1 -> sk1". The type of
the second `id' in the above equation becomes "sk2 -> sk2" -- thus the
types are not equal. Indeed, if we ask GHC to show us the inferred
type of the above expression

   *FP> :t type'eq id id
   type'eq id id :: forall a a1 b. (TypeEq (a -> a) (a1 -> a1) b) => b

we can see the Skolemization very clearly.

OTH,
   *FP> let f x = (x,x) in case f id of (u,v) -> type'eq u v
   HTrue
because
   *FP> :t let f x = (x,x) in case f id of (u,v) -> type'eq u v
   let f x ...  :: forall a b. (TypeEq (a -> a) (a -> a) b) => b

that is, the types of both `id' are unified (yet they remain unground)

* Two type-to-term reifiers

As we have mentioned, we introduce two type reifiers. The first one converts
a type to the following term:

> data Term = Var Int | L Int Term | A Term Term -- deriving Show

The custom Show instance is at the end of this message. The second reifier
converts a type to a true Haskell term. The difference between the reifiers
is akin to the difference between a homogeneous list and a heterogeneous
list (HList). The former reifier is far easier to explain.

* Function types as logic programs. Solving type habitation by SLD resolution

To solve the type habitation problem -- to find a term (proof) for a
proposition expressed as a type -- we use SLD resolution. It may be
helpful to think of the whole process as converting a type
(proposition) into a Horn-clause logical program, and solving that
program using a Prolog-style evaluator. Here are a few examples of
types and the corresponding logical programs.

The type a->b->a after applying the implication elimination rule twice
yields the following program:

        t2t(a, var(1)).
        t2t(b, var(2)).
        ?- t2t(a,X), Term = lambda(1,(lambda(2,X))).
        % solution: lambda(1, lambda(2, var(1))), or \x y -> x

The type "(b -> c) -> (a -> b) -> a -> c"  (of the composition function)
corresponds to the following program:

        t2t(c,app(var(1),X)) :- t2t(b,X).
        t2t(b,app(var(2),X)) :- t2t(a,X).
        t2t(a,var(3)).
        ?- t2t(c,X), Term = lambda(1,lambda(2,lambda(3,X))).
        % solution:
        % lambda(1, lambda(2, lambda(3, app(var(1), app(var(2), var(3))))))
        % or \x y z -> x (y z)


The type of one of the X combinators [1] (which is an improper combinator)
        forall t a b c t1 t2 t3.
        (((a -> b -> c) -> (a -> b) -> a -> c) -> (t3 -> t1 -> t2 -> t3) -> t)
        -> t

corresponds to the logical program

        t2t(t, app(app(var(1),X),Y)) :- t2t(u1,X), t2t(u2,Y).

        % u1 denotes (a -> b -> c) -> (a -> b) -> a -> c
        t2t(u1,X) :- t2t(c,Y), X = lambda(3,lambda(4,lambda(5,Y))).
        t2t(c,app(app(var(3),X),Y)) :- t2t(a,X), t2t(b,Y).
        t2t(b,app(var(4),X)) :- t2t(a,X).
        t2t(a,var(5)).

        % u2 denotes t3 -> t1 -> t2 -> t3
        t2t(u2,X) :- t2t(t3,Y), X = lambda(6,lambda(7,lambda(8,Y))).
        t2t(t3,var(6)).
        t2t(t1,var(7)).
        t2t(t2,var(8)).

        ?- t2t(t,X), Term = lambda(1,X).

The solution to the latter is, when printed nicely,
    \f -> f (\a b c -> (a c) (b c)) (\x y z -> x)


We construct such programs and solve them with the ordinary SLD resolution
-- only using Haskell typeclasses rather than Prolog.

[1] Jeroen Fokker, The Systematic Construction of a One-combinator Basis for
Lambda-Terms.
Formal Aspects of Computing 4 (1992), pp. 776-780.
http://www.cs.uu.nl/people/jeroen/article/combinat/combinat.ps


* More examples

The reification function takes, as one argument, an environment -- or
the initial assumption. Oftentimes it is this:

> gamma0 = G HNil 0

Other environments may be used to reify *open* terms.

We start with a few simple examples:

> test1 = let term = \x y -> x in reify term gamma0
> test2 = let term = \x y -> y in reify term gamma0
> test3 = let term = \f x y -> f x in reify term gamma0
>
> -- \f x y -> f y x
> test4 = reify (__::(t -> t1 -> t2) -> t1 -> t -> t2) gamma0
>
> -- \f g a b -> f (g a b)
> test5 = let term = (.) (.) (.) in reify (undefined `asTypeOf` term) gamma0
>
> pz = (((. head . uncurry zip . splitAt 1 . repeat) . uncurry) .) . (.) . flip
> test_pz = reify (undefined `asTypeOf` pz) gamma0
>
> -- \f g h x y -> f (g x) (h y)
> test7 = let term = ((flip . ((.) .)) .) . (.) in reify term gamma0

More complex are improper combinators:

> -- \f -> f (\x -> x)
> test6 = reify (__::((a->a)->b) -> b) gamma0
>
> -- X combinators
> {-- commented out to speed up loading
> test8 = let term = \a b c d -> c d (a (\x -> d))
>         in reify (undefined `asTypeOf` term) gamma0
>
> test9 = reify (undefined::(((a -> b -> c) -> (a -> b) -> a -> c) ->
>                            (t3 -> t1 -> t2 -> t3) -> t) -> t) gamma0
> --}

Other terms to try:
   ((((.) . (.)) . (.)) . (.))
   ((flip .) . (((flip .) .) . (((.) . (.)) . (((.) . (.)) .))))
   reify const gamma0
   reify (.) gamma0
   reify (asTypeOf __ (.)) gamma0

> term1B = reify (undefined:: t4 -> (t3 -> t4 -> t5 -> t1) -> t6 -> t3 ->
>                  (t4 -> t -> t -> t5 -> t1 -> t2) -> t -> t5 -> t2) gamma0

The type a->b however is not inhabitable: If we uncomment the following,

> --test_not_derivable = reify (__::a -> b) gamma0

we get the type error:
    No instances for (Resolve (Gamma (:*: (T2T (:*: a HNil)) HNil)) assum',
                      HLookup rt HNil (:*: rt assum))
      arising from use of `reify' at ...

That is, a |- rt is unprovable.

* Code

** Preliminaries

An abbreviated notation for undefined, which shall occur very often

> __ = __

Heterogeneous lists

> data HTrue
> data HFalse
> instance Show HTrue  where show _ = "HTrue"
> instance Show HFalse where show _ = "HFalse"

> data HNil = HNil deriving Show
> data HCons a b = HCons a b deriving Show

> -- Syntax sugar from the HList library. Thanks to Ralf Laemmel
> infixr 2 :*:
> infixr 2 .*.
> type e :*: l = HCons e l
> a .*. b = HCons a b

Environment: hs are hypotheses: an HList of T2T, each of which states one
assumption: an association of a type to a term

> data Gamma hs = G hs Int deriving Show
> newtype T2T t = T2T Term deriving Show

Converting an implication to a type list of assumptions in inverse order

> class I2L t tl | t -> tl where
>     i2l :: t -> tl
>     i2l = undefined -- it's a type-level function
>
> instance (IsFunction t flag, I2L' flag t tl)
>     => I2L t tl
>
> class I2L' flag t tl | flag t -> tl
>
> instance I2L' HFalse t (t :*: HNil)
>
> instance (I2L x tlx, I2L y tly, HAppend tly (tlx :*: HNil) tl)
>     => I2L' HTrue (x->y) tl
>
> ti1 = i2l (__::a->b->c)
> ti2 = i2l (__::(a->b)->a->b)
> ti3 = i2l (__::(a -> b -> c) -> (a -> b) -> a -> c)


The main reification class

> class Reify t gamma where
>     reify :: t -> gamma -> Term
>
> instance (IsFunction t flag, I2L' flag t (rt :*: at),
>           AddHyp gamma at gamma',
>           Resolve gamma' ((rt :*: HNil) :*: HNil))
>     => Reify t gamma where
>     reify t gamma = let (varlist, gamma') = add'hyp gamma (__::at) []
>                     in foldr (\ (Var v) s -> L v s )
>                            (resolve gamma' (((__::rt) .*. HNil) .*. HNil) [])
>                            varlist

Label top-level assumptions with variables

> class AddHyp gamma tl gamma' | gamma tl -> gamma' where
>     add'hyp :: gamma -> tl -> [Term] -> ([Term],gamma')
>
> instance AddHyp gamma HNil gamma where
>     add'hyp g _ varlist = (varlist,g)
>
> instance AddHyp (Gamma ((T2T t) :*: hs)) r gamma
>     => AddHyp (Gamma hs) (t :*: r) gamma where
>     add'hyp (G hs varcount) _ varlist =
>       let v   = (Var varcount)
>           hs' = ((T2T v)::T2T t) .*. hs
>       in add'hyp (G hs' (varcount + 1)) (__::r) (v:varlist)


** The SLD resolution algorithm

> class Resolve gamma goals where
>     resolve :: gamma -> goals -> [Term] -> Term
>
> instance Resolve gamma HNil where
>     resolve _ _ pt = foldr1 (flip A) pt
>
> instance (HLookup g hs (g :*: assum),
>           HReverse assum assum',
>           Resolve (Gamma hs) assum',
>           Resolve (Gamma hs) gr)
>     => Resolve (Gamma hs) ((g :*: HNil) :*: gr) where
>     resolve gamma@(G hs _) _ pt =
>       let T2T t1 = hlookup (__::g) hs
>       in resolve gamma (__::gr) ((resolve gamma (__::assum') [t1]) : pt)
>
> instance (AddHyp (Gamma hs) (gc :*: gcr) gamma',
>           AddHyp (Gamma ((T2T gc) :*: hs)) gcr gamma',
>           Resolve gamma' ((g :*: HNil) :*: HNil),
>           Resolve (Gamma hs) gr)
>     => Resolve (Gamma hs) ((g :*: gc :*: gcr) :*: gr) where
>     resolve gamma@(G hs _) _ pt =
>       let t1 = let (varlist, gamma'::gamma') =
>                        add'hyp gamma (__::(gc :*: gcr)) []
>                in foldr (\ (Var v) s -> L v s )
>                         (resolve gamma' (((__::g) .*. HNil) .*. HNil) [])
>                         varlist
>       in resolve gamma (__::gr) (t1 : pt)

Lookup in the `associative' type-indexed list

> class HLookup t l w | t l -> w where
>     hlookup :: t -> l -> T2T w
>
> instance (TypeEq t t' flag, HLookup' flag t ((T2T (t' :*: at)) :*: r) w)
>     => HLookup t ((T2T (t' :*: at)) :*: r) w where
>     hlookup = hlookup' (undefined::flag)
>
> class HLookup' flag t l rt | flag t l -> rt where
>     hlookup' :: flag -> t -> l -> T2T rt
>
> instance HLookup' HTrue t ((T2T (t :*: at)) :*: r) (t :*: at) where
>     hlookup' _ _ (HCons t _) = t
>
> instance HLookup t r w => HLookup' HFalse t ((T2T t') :*: r) w where
>     hlookup' _ t (HCons _ r) = hlookup t r



> class HAppend l1 l2 l | l1 l2 -> l where
>     happend :: l1 -> l2 -> l
> instance HAppend HNil l2 l2 where
>     happend _ l2 = l2
> instance HAppend l1 l2 l => HAppend (a :*: l1) l2 (a :*: l) where
>     happend (HCons a l1) l2 = a .*. (happend l1 l2)

> class HReverse l1 l2 | l1 -> l2
> instance HReverse HNil HNil
> instance (HReverse l1 l1', HAppend l1' (a :*: HNil) l2)
>     => HReverse (a :*: l1) l2


** Show Terms in a nice way

> instance Show Term where
>     show (Var i) =
>       case divMod i 26 of
>                        (0,i) -> simple_var i
>                        (j,i) -> simple_var i ++ (show j)
>         where simple_var i = ["yphdcbaeijklmnofqrstuvwxgz" !! i]
>     show (A e v@(Var i)) = show e ++ " " ++ show v
>     show (A e1 e2) = show e1 ++ " " ++ "(" ++ show e2 ++ ")"
>     show (L i e) = show' [i] e
>       where show' vars (L j e) = show' (j:vars) e
>             show' vars e = "\\" ++ unwords (map (show.Var) $ reverse vars) ++
>                            " -> " ++ show e


* The second reifier: from types to bona fide Haskell terms

The second reifier is a `lifted' version of the first one. Wherever we
used regular Haskell lists before, we use HLists now.

> newtype T2TT tl t = T2TT t deriving Show
>
> class RR t gamma where
>     rr :: t -> gamma -> t
>
> instance (IsFunction t flag, RR' flag t gamma)
>     => RR t gamma where
>     rr = rr' (__::flag)
>
> class RR' flag t gamma where
>     rr' :: flag -> t -> gamma -> t
>
> instance (IsFunction x flagx, I2L' flagx x tlx,
>           IsFunction y flagy, RR' flagy y ((T2TT tlx x) :*: gamma))
>     => RR' HTrue (x->y) gamma where
>     rr' _ _ gamma = \ (v::x) -> rr' (__::flagy) (__::y)
>                                   (((T2TT v)::T2TT tlx x) .*. gamma)
>
> instance (RResolve gamma ((t :*: HNil) :*: HNil) HNil t)
>     => RR' HFalse t gamma where
>     rr' _ _ gamma = rresolve gamma (((__::t) .*. HNil) .*. HNil) HNil
>
>
> class RResolve gamma goals tl t | gamma goals tl -> t where
>     rresolve :: gamma -> goals -> tl -> t
>
> instance RResolve gamma HNil (t :*: HNil) t where
>     rresolve _ _ (HCons t HNil) = t -- foldr1 (flip A) pt
>
> instance RResolve gamma HNil (t1 :*: tr) (t->r)
>     => RResolve gamma HNil (t :*: t1 :*: tr) r where
>     rresolve g _ (HCons t r) = (rresolve g HNil r) t
>
> instance (RHLookup g gamma (T2TT (g :*: assum) g'),
>           HReverse assum assum',
>           RResolve gamma assum' (g' :*: HNil) ra,
>           RResolve gamma gr (ra :*: pt) t)
>     => RResolve gamma ((g :*: HNil) :*: gr) pt t where
>     rresolve gamma _ pt =
>       let T2TT t1 = rhlookup (__::g) gamma
>           ra :: ra = rresolve gamma (__::assum') (t1 .*. HNil)
>       in rresolve gamma (__::gr) (ra .*. pt)
> -- the instance for improper combinators is left as an exercise to the reader

> -- Lookup in the `associative' type-indexed list
> class RHLookup t l w | t l -> w where
>     rhlookup :: t -> l -> w
>
> instance (TypeEq t t' flag,RHLookup' flag t ((T2TT (t' :*: at) tt') :*: r) w)
>     => RHLookup t ((T2TT (t' :*: at) tt') :*: r) w where
>     rhlookup = rhlookup' (__::flag)
>
> class RHLookup' flag t l w | flag t l -> w where
>     rhlookup' :: flag -> t -> l -> w
>
> instance RHLookup' HTrue t ((T2TT (t :*: at) tt) :*: r)
>                            (T2TT (t :*: at) tt) where
>     rhlookup' _ _ (HCons t _) = t
>
> instance RHLookup t r w => RHLookup' HFalse t ((T2TT tl' t') :*: r) w where
>     rhlookup' _ t (HCons _ r) = rhlookup t r


A few tests:

> rtest1 = let f (x::a) (y::b) ::a = rr undefined HNil x y
>        in f 1 2
> rtest2 = let f x y = rr (undefined::a->b->b) HNil x y
>        in f 1 2
>
> -- \f x y -> f x :: forall t t1 t2. (t -> t1) -> t -> t2 -> t1
> rtest3 = let t f x y = rr (undefined::(t -> t1) -> t -> t2 -> t1) HNil f x y
>        in t Just 1 'c'
>
> rtest4 f g = rr (undefined::(b -> c) -> (a -> b) -> a -> c) HNil f g
> -- *HC> rtest4 (:[]) (\x -> (True,x)) 10
> -- [(True,10)]
> -- must be truly polymorphic!


* Equality and deconstruction of type schemas

> class IsFunction a b | a -> b
> instance TypeCast f HTrue => IsFunction (x->y) f
> instance TypeCast f HFalse => IsFunction a f
>
> -- literally lifted from the HList library
> class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
> class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
> class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
> instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
> instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
> instance TypeCast'' () a a where typeCast'' _ x  = x
>
> class TypeEq' () x y b => TypeEq x y b | x y -> b
>     where type'eq :: x -> y -> b
>           type'eq _ _ = undefined::b
> class TypeEq' q x y b | q x y -> b
> class TypeEq'' q x y b | q x y -> b
> instance TypeEq' () x y b => TypeEq x y b
> instance TypeCast b HTrue => TypeEq' () x x b
> instance TypeEq'' q x y b => TypeEq' q x y b
> instance TypeEq'' () x y HFalse

-}