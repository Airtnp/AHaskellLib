-- Ref
    -- Terms: ref(ref create) / !(deref) / :=(assignment) / l(ref pos) (13.2)
    -- Types: Ref type / Storage type
-- Change storage ===> Change environment (13.3)

-- Exception
    -- Terms: Γ⊢undefined : T / try .. with ... / raise t

-- Subtyping (λc)
    -- Types: Top (T <: Top) / Bottom (Bot <: T)
    -- Rules: width / depth / permutation

-- Recursive type
    -- NatList = \mu X. <nil : Unit, cons : {Nat, X}>
    -- \mu X.T: 满足 X = [X -> \mu X.T]T 的无穷类型

    -- equi-recursive
    -- iso-recursive

    -- Fold [\mu X.T] -> \mu X.T -> [X -> \mu X.T]T
        -- U = \mu X.T   \Gamma |- t : [X -> U] T
        -- ---------------------------------------
        -- \Gamma |- fold [U] t : U
    
        -- NLBody = <nil : Unit, cons : {Nat, NatList}>
        -- nil = fold [NatList] (<nil = Unit> as NLBody)
        -- cons = \n : Nat -> \l: NatList -> fold [NatList] <cons = {n, l}> as NLBody
    
        -- Unfold [\mu X.T] -> [X -> \mu X.T] T -> \mu X.T
        -- U = \mu X.T   \Gamma |- t : U
        -- ---------------------------------------
        -- \Gamma |- unfold [U] t : [X -> U]T

-- Polymorphism
    -- In general, the context now associates each free variable with a type scheme, not just a type.
    -- \Gamma |- t : S | C
    -- (\sigma, T) ==> \sigma satisfy C and \sigma S = T
    -- unificaiton
    -- principle types
    -- let polymorphism
    -- 1. We use the constraint typing rules to calculate a type S1 and a set C1 of associated constraints for the right-hand side t1.
    -- 2. We use unification to find a most general solution σ to the constraints C1 and apply σ to S1 (and Γ) to obtain t1’s principal type T1.
    -- 3. We generalize any variables remaining in T1. If X1. . .Xn are the remaining variables, we write ∀X1...Xn.T1 for the principal type scheme of t1
    -- 4. We extend the context to record the type scheme ∀X1...Xn.T1 for the bound variable x, and start typechecking the body t2. In general, the context now associates each free variable with a type scheme, not just a type.
    -- 5. Each time we encounter an occurrence of x in t2, we look up its type scheme ∀X1...Xn.T1. We now generate fresh type variables Y1...Yn and use them to instantiate the type scheme, yielding [X1 , Y1, ..., Xn , Yn]T1, which we use as the type of x.

-- System F
    -- \lambda X.t t[T] type abstraction
    -- Universal type
    -- Existential type
        -- {*S, t} as {\exists X, {t : X}} S satisfy X
        -- {\exists X, T} === \forall Y. (\forall X. T -> Y) -> Y
        -- {*S, t} as {\exists X, T} === \lambda Y. \lambda f : (\forall X. T -> Y). f[S] t
        -- let {X, x} = t1 in t2 === t1 [T2] (\lambda X. \lambda x : T11. t2)
        -- counterADT = {*Nat, {new = 1, get = \i:Nat i, inc = \i:Nat, succ(i)}}
            -- counterADT : {\exists Counter, {new:Counter, get:Counter->Nat}, ...}
            -- let {Counter, counter} = counterADT in ....
            -- Counter is ADT, counterADT is OOP

-- System F_{<:}
    -- \lambda X <: T. t
    -- \forall X <: T. T
    -- \Gamma, X <: T

-- System F_{\omega}
    -- X: Kind
    -- \lambda X::K.T
    -- \Gamma, X::K
    -- * / K -> K
    -- \forall X :: K. T ===>(K == *) \forall X . T
    -- |\exists X :: K, T| ===>(K == *) |\exists X, T|

-- Lambda Cube
    -- ref: https://cs.stackexchange.com/questions/49189/what-terms-type-systems-exclude/49381#49381
    -- ref: https://cstheory.stackexchange.com/questions/36054/how-do-you-get-the-calculus-of-constructions-from-the-other-points-in-the-lambda/36058#36058
    -- terms depend on terms (normal functional programming) \x -> x
    -- terms depend on types (polymorphism) Head : [X] -> X
    -- types depend on types (type operator / type families) List<T> : X -> [X]
    -- types depend on terms (dependent types) Array<U, N> : N -> U^N

    -- λ→ (Simply-typed lambda calculus)
    -- λω_ (STLC + higher-kinded type operators)
        -- \lambda a : K. t
    -- λ2 (System F: STLC + poly)
        -- ∀a:k. A
        -- Λa:k. e + e [A]
    -- λω (System F-omega: STLC + poly(parametric) + type operator)
    -- λP (LF: STLC + dependent type)
        -- Πx:A. B(x) (arugment in return type)
    -- λP2 (no special name: + dt + poly)
    -- λPω_ (no special name: + dt + type operator)
    -- λPω (the Calculus of Constructions: all)

    -- what's not include is subtyping (X <: Y)
    -- HM is part of System F

-- System F^{\omega}_{<:}