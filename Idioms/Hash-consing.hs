-- Hash consing
-- ref: https://wiki.haskell.org/MemoisingCafs
-- ref: https://wiki.haskell.org/Tying_the_Knot
-- some-blog: http://mergeconflict.com/tying-the-knot-redux/

-- Use a table of already-built structures to increase sharing.

-- 1. Memoising CAFs

-- Memoising constructor functions gives you HashConsing, and you can sometimes use MemoisingCafs to implement that.

-- The MemoisingCafs idiom also supports recursion.

-- 3x+1 problem
wonderous :: Integer -> Integer
wonderous 1 = 0
wonderous x
  | even x    = 1 + wonderous (x `div` 2)
  | otherwise = 1 + wonderous (3*x+1)

-- we can memoise some of the domain using an array CAF

wonderous2 :: Integer -> Integer
wonderous2 x
  | x <= maxMemo = memoArray ! x
  | otherwise    = wonderous2' x
  where
        maxMemo = 100
        memoArray = array (1,maxMemo)
                        [ (x, wonderous2' x) | x <- [1..maxMemo] ]
 
        wonderous2' 1 = 0
        wonderous2' x
          | even x    = 1 + wonderous2 (x `div` 2)
          | otherwise = 1 + wonderous2' (3*x+1)

-- When using this pattern in your own code, note carefully when to call the memoised version (wonderous2 in the above example) and when not to. In general, the partially memoised version (wonderous2' in the above example) should call the memoised version if it needs to perform a recursive call. However, in this instance, we only memoize for small values of x, so the branch of the recursion that passes a larger argument need not bother checking the memo table. (This does slow the array initialization, however.) Thanks to LazyEvaluation, we can even memoise an infinite domain, though we lose constant time lookup. This data structure is O(log N):

type MemoTable a = [(Integer, BinTree a)]
data BinTree a = Leaf a | Node Integer (BinTree a) (BinTree a)
 
wonderous3 :: Integer -> Integer
wonderous3 x
  = searchMemoTable x memoTable
  where
        memoTable :: MemoTable Integer
        memoTable = buildMemoTable 1 5
 
        buildMemoTable n i
            = (nextn, buildMemoTable' n i) : buildMemoTable nextn (i+1)
            where
                nextn = n + 2^i
 
                buildMemoTable' base 0
                    = Leaf (wonderous3' base)
                buildMemoTable' base i
                    = Node (base + midSize)
                           (buildMemoTable' base (i-1))
                           (buildMemoTable' (base + midSize) (i-1))
                    where
                        midSize = 2 ^ (i-1)
 
        searchMemoTable x ((x',tree):ms)
            | x < x'    = searchMemoTree x tree
            | otherwise = searchMemoTable x ms
 
        searchMemoTree x (Leaf y) = y
        searchMemoTree x (Node mid l r)
            | x < mid   = searchMemoTree x l
            | otherwise = searchMemoTree x r
 
        wonderous3' 1 = 0
        wonderous3' x
          | even x    = 1 + wonderous3 (x `div` 2)
          | otherwise = 1 + wonderous3 (3*x+1)

-- Naturally, these techniques can be combined, say, by using a fast CAF data structure for the most common part of the domain and an infinite CAF data structure for the rest.


-- 2. Tying the Knot

-- In a language like Haskell, where Lists are defined as Nil | Cons a (List a), creating data structures like cyclic or doubly linked lists seems impossible. However, this is not the case: laziness allows for such definitions, and the procedure of doing so is called tying the knot. The simplest example:

cyclic = let x = 0 : y
             y = 1 : x
         in x

-- This example illustrates different ways to define recursive data structures. To demonstrate the different techniques we show how to solve the same problem---writing an interpreter for a simple programming language---in three different ways. This is a nice example because, (i) it is interesting, (ii) the abstract syntax of the language contains mutually recursive structures, and (iii) the interpreter illustrates how to work with the recursive structures.

{-
    Direct Recursion

    Syntax
    ~~~~~~

    > type Name = String
    > 
    > data Expr = Let Decl Expr
    >           | App Expr Expr
    >           | Var Name
    >           | Int Int
    >           
    > data Decl = Fun Name [Name] Expr


    Semantics
    ~~~~~~~~~

    > data Val  = IntVal Int | FunVal (Val -> Val)
    > type Env  = [(Name,Val)]
    > 
    > evalExpr :: Expr -> Env -> Val
    > evalExpr expr env = case expr of
    >   Let d e   -> evalExpr e (evalDecl d env ++ env)
    >   App e1 e2 -> case evalExpr e1 env of
    >                  FunVal f -> f (evalExpr e2 env)
    >                  _ -> error "Type error."
    >   Var x     -> case lookup x env of
    >                  Just v  -> v
    >                  Nothing -> error "Undefined variable."
    >   Int x     -> IntVal x
    > 
    > evalDecl :: Decl -> Env -> Env
    > evalDecl decl env = case decl of
    >   Fun f xs e -> [(f,fun env xs)]
    >     where fun env (x:xs) = FunVal (\v -> fun ((x,v):env) xs)
    >           fun env []     = evalExpr e env


    Examples
    ~~~~~~~~

    > test e  = evalExpr 
    >         ( Let (Fun "id" ["x"] $ Var "x") 
    >         $ Let (Fun "const" ["x","y"] $ Var "x") e) []
    >
    > ex1 = test (Var "id" `App` Int 2)
    > ex2 = test (Var "const" `App` Int 2 `App` Int 3)
    > ex3 = test (Var "const" `App` Var "id" `App` Int 2 `App` Int 3)
    
    > instance Show Val where
    >   show val = case val of
    >     IntVal x -> show x
    >     FunVal _ -> "<function>"
    > 


-}

{-

    Tying the Kont

 \{-# OPTIONS -fglasgow-exts #-\}

    Parameterized Syntax
    ~~~~~~~~~~~~~~~~~~~~

    > type Name       = String
    > 
    > data Expr e d   = Let d e
    >                 | App e e
    >                 | Var Name
    >                 | Int Int
    > 
    > data Decl e d   = Fun Name [Name] e


    Parameterized Semantics 
    ~~~~~~~~~~~~~~~~~~~~~~~

    > data Val        = IntVal Int | FunVal (Val -> Val)
    > type Env        = [(Name,Val)]
    > 
    > class Eval e d | e -> d, d -> e where 
    >   expr :: e -> Env -> Val
    >   decl :: d -> Env -> Env 
    > 
    > instance (Eval e d) => Eval (Expr e d) (Decl e d) where
    >   expr e env = case e of
    >     Let d e   -> expr e (decl d env ++ env)
    >     App e1 e2 -> case expr e1 env of
    >                    FunVal f -> f (expr e2 env)
    >                    _ -> error "Type error."
    >     Var x     -> case lookup x env of
    >                    Just v  -> v
    >                    Nothing -> error "Undefined variable."
    >     Int x     -> IntVal x
    > 
    >   decl d env = case d of
    >     Fun f xs e -> [(f,args env xs)]
    >       where args env (x:xs) = FunVal (\v -> args ((x,v):env) xs)
    >             args env []     = expr e env


    Language 1: Tying the Knot 
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    > newtype Expr1 = E1 (Expr Expr1 Decl1)
    > newtype Decl1 = D1 (Decl Expr1 Decl1)
    > 
    > instance Eval Expr1 Decl1 where 
    >   expr (E1 e) env = expr e env
    >   decl (D1 e) env = decl e env
    
    Examples:

    > var1 x      = E1 $ Var x
    > int1 x      = E1 $ Int x
    > app1 f x    = E1 $ App f x
    > let1 d e    = E1 $ Let d e
    > fun1 f xs e = D1 $ Fun f xs e
    >  
    > test1 e = expr
    >         ( let1 (fun1 "id" ["x"]        $ var1 "x") 
    >         $ let1 (fun1 "const" ["x","y"] $ var1 "x") e) []
    > 
    > ex1     = test1 $ var1 "id"    `app1` int1 2
    > ex2     = test1 $ var1 "const" `app1` int1 2    `app1` int1 3
    > ex3     = test1 $ var1 "const" `app1` var1 "id" `app1` int1 2 `app1` int1 3


    Language 2: Tying the Knot with an Extension
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

    > data Expr2    = E2 (Expr Expr2 Decl2)
    >               | Add Expr2 Expr2
    > newtype Decl2 = D2 (Decl Expr2 Decl2)
    > 
    > instance Eval Expr2 Decl2 where
    >   expr (E2 e) env     = expr e env
    >   expr (Add e1 e2) env = case (expr e1 env, expr e2 env) of
    >                            (IntVal x, IntVal y) -> IntVal (x+y)
    >                            _ -> error "Type error."
    >   decl (D2 d) env = decl d env
    
    Examples:

    > var2 x      = E2 $ Var x
    > int2 x      = E2 $ Int x
    > app2 f x    = E2 $ App f x
    > let2 d e    = E2 $ Let d e
    > fun2 f xs e = D2 $ Fun f xs e
    > 
    > test2 e = expr
    >         ( let2 (fun2 "id" ["x"]        $ var2 "x") 
    >         $ let2 (fun2 "const" ["x","y"] $ var2 "x") e) []
    > 
    > ex4     = test2 $ var2 "id"    `app2` int2 2
    > ex5     = test2 $ var2 "const" `app2` int2 2    `app2` int2 3
    > ex6     = test2 $ var2 "const" `app2` var2 "id" `app2` int2 2 `app2` int2 3
    > ex7     = test2 $ var2 "id" `app2` (int2 3 `Add` int2 7)


    > instance Show Val where
    >   show val = case val of
    >     IntVal x -> show x
    >     FunVal _ -> error "<function>"

-}

{-

    Tying the Knot with GADT

    > \{-# OPTIONS -fglasgow-exts #-\}

    "A Pattern for Almost Compositional Functions"
    Bjorn Bringert, Aarne Ranta (Chalmers)


    Syntax
    ~~~~~~

    > type Name = String
    > 
    > data Expr
    > data Decl
    > 
    > data Term term cat where
    >   Let :: term Decl -> term Expr -> Term term Expr
    >   App :: term Expr -> term Expr -> Term term Expr 
    >   Var :: Name -> Term term Expr 
    >   Int :: Int  -> Term term Expr 
    >   Fun :: Name -> [Name] -> term Expr -> Term term Decl


    (Paramtrized) Semantics
    ~~~~~~~~~~~~~~~~~~~~~~~

    > data Val cat where
    >   IntVal :: Int -> Val Expr
    >   FunVal :: (Val Expr -> Val Expr) -> Val Expr
    >   Env    :: Env -> Val Decl
    > 
    > type Env = [(Name, Val Expr)]
    > 
    > class Eval term where
    >   eval :: term cat -> Env -> Val cat
    > 
    > instance (Eval term) => Eval (Term term) where
    >   eval term env = case term of
    >     Let d e     -> case eval d env of
    >                      Env env' -> eval e (env' ++ env) 
    >                      _ -> error "Type error (in GHC)."
    >     App e1 e2   -> case eval e1 env of
    >                      FunVal f -> f (eval e2 env)
    >                      _ -> error "Type error."
    >     Var x       -> case lookup x env of
    >                      Just v  -> v
    >                      Nothing -> error "Undefined variable."
    >     Int x       -> IntVal x
    >   
    >     Fun f xs e  -> Env [(f,args env xs)]
    >         where args env (x:xs) = FunVal (\v -> args ((x,v):env) xs)
    >               args env []     = eval e env


    Language 1: Tying the Knot
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    > newtype Lang1 cat = L1 (Term Lang1 cat)
    > 
    > instance Eval Lang1 where
    >   eval (L1 term) env = eval term env
    
    Examples:

    > var1 x      = L1 $ Var x
    > int1 x      = L1 $ Int x
    > app1 f x    = L1 $ App f x
    > let1 d e    = L1 $ Let d e
    > fun1 f xs e = L1 $ Fun f xs e
    >  
    > test1 e = eval
    >         ( let1 (fun1 "id" ["x"]        $ var1 "x") 
    >         $ let1 (fun1 "const" ["x","y"] $ var1 "x") e) []
    > 
    > ex1     = test1 $ var1 "id"    `app1` int1 2
    > ex2     = test1 $ var1 "const" `app1` int1 2    `app1` int1 3
    > ex3     = test1 $ var1 "const" `app1` var1 "id" `app1` int1 2 `app1` int1 3


    Language 2: Tying the Knot with an Extension
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    > data Lang2 cat where 
    >   L2  :: Term Lang2 cat -> Lang2 cat
    >   Add :: Lang2 Expr -> Lang2 Expr -> Lang2 Expr
    > 
    > instance Eval Lang2 where
    >   eval (L2 term) env = eval term env
    >   eval (Add e1 e2) env = case (eval e1 env, eval e2 env) of
    >                           (IntVal x, IntVal y) -> IntVal (x+y)
    >                           _ -> error "Type error."
    
    Examples:

    > var2 x      = L2 $ Var x
    > int2 x      = L2 $ Int x
    > app2 f x    = L2 $ App f x
    > let2 d e    = L2 $ Let d e
    > fun2 f xs e = L2 $ Fun f xs e
    > 
    > test2 e = eval
    >         ( let2 (fun2 "id" ["x"]        $ var2 "x") 
    >         $ let2 (fun2 "const" ["x","y"] $ var2 "x") e) []
    > 
    > ex4     = test2 $ var2 "id"    `app2` int2 2
    > ex5     = test2 $ var2 "const" `app2` int2 2    `app2` int2 3
    > ex6     = test2 $ var2 "const" `app2` var2 "id" `app2` int2 2 `app2` int2 3
    > ex7     = test2 $ var2 "id" `app2` (int2 3 `Add` int2 7)


    > instance Show (Val cat) where
    >   show term = case term of
    >     IntVal x -> show x
    >     FunVal _ -> "<function>"
    >     Env xs   -> show xs


-}


-- How to build a cyclic data structure.

-- Circular, doubly-linked-list

data DList a = DLNode (DList a) a (DList a)
 
mkDList :: [a] -> DList a
 
mkDList [] = error "must have at least one element"
mkDList xs = let (first,last) = go last xs first
             in  first
 
  where go :: DList a -> [a] -> DList a -> (DList a, DList a)
        go prev []     next = (next,prev)
        go prev (x:xs) next = let this        = DLNode prev x rest
                                  (rest,last) = go this xs next
                              in  (this,last)
 
takeF :: Integer -> DList a -> [a]
takeF 0     _                 = []
takeF (n+1) (DLNode _ x next) = x : (takeF n next)
 
takeR :: Show a => Integer -> DList a -> [a]
takeR 0     _                 = []
takeR (n+1) (DLNode prev x _) = x : (takeR n prev)

-- The problem with this is it won't make a truly cyclic data structure, rather it will constantly be generating the rest of the list. To see this use trace (in Debug.Trace for GHC) in mkDList (e.g. mkDList xs = trace "mkDList" $ ...) and then takeF 10 (mkDList "a"). Add a trace to mkDList or go or wherever you like in the other version and note the difference.

-- DFA recursive DS

-- data DirectDfa a
--   = DirectState Bool [(a, DirectDfa a)]

-- -- Then we can just execute the DFA like this:

-- runDfa :: (Eq a) => DirectDfa a -> [a] -> Bool
-- runDfa (DirectState final trans) []
--   = final
-- runDfa (DirectState final trans) (x:xs)
--   = case [ s | (x',s) <- trans, x == x' ] of
--         []    -> False
--         (s:_) -> runDfa s xs


{-

    Transformations of cyclic graphs and the Credit Card Transform

    Cycles certainly make it difficult to transform graphs in a pure
    non-strict language. Cycles in a source graph require us to devise a
    way to mark traversed nodes -- however we cannot mutate nodes and
    cannot even compare nodes with a generic ('derived') equality
    operator. Cycles in a destination graph require us to keep track of
    the already constructed nodes so we can complete a cycle. An obvious
    solution is to use a state monad and IORefs. There is also a
    monad-less solution, which is less obvious: seemingly we cannot add a
    node to the dictionary of already constructed nodes until we have
    built the node. This fact means that we cannot use the updated
    dictionary when building the descendants of the node -- which need the
    updated dictionary to link back. The problem can be overcome however
    with a _credit card transform_ (a.k.a. "buy now, pay later"
    transform). To avoid hitting the bottom, we just have to "pay" by the
    "due date".

    For illustration, we will consider the problem of printing out a
    non-deterministic finite automaton (NFA) and transforming it into a
    deterministic finite automaton (DFA). Both NFA and DFA are represented
    as cyclic graphs. The problem has been discussed on the
    Haskell/Haskell-Cafe mailing lists. The automata in question were to
    recognize strings over a binary alphabet.

    > module CCardFA where
    >
    > import Data.List


    A state of an automaton over a binary alphabet is a data structure:

    > data FaState l = 
    >	FaState {label :: l, acceptQ :: Bool, 
    >	         trans0:: [FaState l], 
    >	         trans1:: [FaState l]}

    whose fields have the obvious meaning. Label is used for printing out
    and comparing states. The flag acceptQ tells if the state is
    final. Since an FaState can generally represent a non-deterministic
    automaton, transitions are the _lists_ of states.

    An automaton is then a list of starting states.

    > type FinAu l = [FaState l]

    For example, an automaton equivalent to the regular expression
    "0*(0(0+1)*)*" could be defined as:

    > dom18 = [one]
    >    where one = FaState 1 True [one,two] []
    >	   two = FaState 2 True [two,one] [one,two]

    using the straightforward translation from a regular expression to an NFA.

    We would like to compare and print automata and their states:

    > instance (Ord l,Show l) => Eq (FaState l) where
    >     (FaState l1 _ _ _) == (FaState l2 _ _ _) = l1 == l2

    Printing a FaState however poses a slight problem. For example, the
    state labeled '1' in the automaton dom18 refers to itself. If we
    blindly 'follow the links', we will loop forever. Therefore, we must
    keep track of the already printed states. We need a data structure for
    such an occurrence check, with the following obvious operations:

    > class OCC occ where
    >     empty:: occ a
    >     seenp:: (Eq a) => a -> occ a -> Bool -- occurrence check predicate
    >     put::  a -> occ a -> occ a           -- add an item
        
    In this article, we realize such a data structure as a list. In the
    future, we can pull in something fancier from the Edison collection:

    > instance OCC [] where
    >     empty = []
    >     seenp = elem
    >     put = (:)
        
    We are now ready to print an automaton. To be more precise, we
    traverse the corresponding graph depth-first, pre-order, and keep
    track of the already printed states. A 'states_seen' datum accumulates
    the shown states, so we can be sure we print each state only once and
    thus avoid the looping.

    > instance (Ord l,Show l) => Show (FaState l) where
    >     show state = "{@" ++ showstates [state] (empty::[FaState l]) "@}"
    >          where
    >            -- showstates worklist seen_states suffix
    >          showstates [] states_seen suffix = suffix
    > 	   showstates (st:rest) states_seen suffix
    > 	       | st `seenp` states_seen = showstates rest states_seen suffix
    > 	   showstates (st@(FaState l accept t0 t1):rest) states_seen suffix =
    > 	       showstate st 
    > 	             $ showstates (t0++t1++rest) (st `put` states_seen) suffix
    >	   
    >          showstate (FaState l accept t0 t1) suffix 
    > 	             = "{State " ++ (show l) ++ 
    >                    " " ++ (show accept) ++ " " ++ (show $ map label t0) ++
    > 	             " " ++ (show $ map label t1) ++ "}" ++ suffix

    Now, 

    CCardFA> print dom18 -- prints as
    CCardFA> [{@{State 1 True [1,2] []}{State 2 True [2,1] [1,2]}@}]


    The acceptance function for our automata can be written as
    follows. The function takes the list of starting states and the string
    over the boolean alphabet. The function returns True if the string is
    accepted.

    > finAuAcceptStringQ start_states str =
    >         foldr (\l seed -> acceptP l str || seed) False start_states
    >   where acceptP (FaState _ acceptQ _ _) [] = acceptQ
    > 	  acceptP (FaState _ _ t0 t1) (s:rest) = 
    > 	          finAuAcceptStringQ (if s then t1 else t0) rest

    To test the automata, we can try

    > test1= finAuAcceptStringQ dom18 $ map (>0) [0,1,0,1]
    > test2= finAuAcceptStringQ dom18 $ map (>0) [1,1,0,1]
    > test3= finAuAcceptStringQ dom18 [True]
    > test4= finAuAcceptStringQ dom18 [False]

    We are now ready to write the NFA->DFA conversion, a determinization
    of an NFA. We implement the textbook algorithm of tracing set of NFA
    states. A state in the resulting DFA corresponds to a list of the NFA
    states. A DFA is generally a cyclic graph, often with cycles of length
    1 (self-referenced nodes). To be able to "link back" as we build DFA
    states, we have to remember the already constructed states. We need a
    data structure, a dictionary of states:

    > class StateDict sd where
    >     emptyd :: sd (l,FaState l)
    >     locate :: (Eq l) => l -> sd (l,FaState l) -> Maybe (FaState l)
    >     putd   :: (l,FaState l) -> sd (l,FaState l) -> sd (l,FaState l)

    For now, we realize this dictionary as an associative list. If performance
    matters, we can use a fancier dictionary from the Edison

    > instance StateDict [] where
    >     emptyd = []
    >     locate = lookup
    >     putd   = (:)

    The work of the NFA->DFA conversion is done by the following function
    determinize_cc. The function takes a list of NFA states, the dictionary
    of the already built states, and returns a pair ([dfa_state],
    updated_dictionary) where [dfa_state] is a singleton list.

    > -- [nfa_state] -> dictionary_of_seen_states -> 
    > --                ([dfa_state],updated_dictionary)
    > -- [dfa_state] is a singleton list
    > determinize_cc states converted_states =
    > 	-- first, check the cache to see if the state has been built already
    >       case  dfa_label `locate` converted_states of
    > 	Nothing -> build_state
    > 	Just dfa_state -> ([dfa_state],converted_states)
    >   where
    >       -- [NFA_labels] -> DFA_labels
    >       det_labels = sort . nub . (map label)
    >       dfa_label  = det_labels states
    >
    >       -- find out NFA-followers for [nfa_state] upon ingestion of 0 and 1
    >       (t0_followers,t1_followers) = 
    > 		foldr (\st (f0,f1) -> (trans0 st ++ f0, trans1 st ++ f1))
    > 		      ([],[]) states
    >       acceptQ'    = or (map acceptQ states)
    >     
    >       -- really build the dfa state and return ([dfa_state],updated_cache)
    >       build_state = let
    > 	   -- note, the dfa_state is computed _below_
    > 	   converted_states1 = (dfa_label,dfa_state) `putd` converted_states
    >          (t0', converted_states2) = 
    > 	           (determinize_cc t0_followers converted_states1)
    > 	   (t1', converted_states3) = 
    > 	           (determinize_cc t1_followers converted_states2)
    >          dfa_state =
    > 	        (FaState dfa_label acceptQ' t0' t1')
    > 	   in ([dfa_state],converted_states3)

    The front end of the NFA->DFA transformer:

    > finAuDeterminize states = fst $ determinize_cc states []

    At the heart of the credit card transform is the phrase from the above code:

    converted_states1 = (dfa_label,dfa_state) `putd` converted_states

    The phrase expresses the addition to the dictionary of the
    'converted_states' of a 'dfa_state' that we haven't built yet. The
    computation of the 'dfa_state' is written 4 lines below the phrase in
    question. Because (,) is non-strict in its arguments and `locate` is
    non-strict in its result, we can get away with a mere promise to
    "pay". Note that the computation of the dfa_state needs t0' and t1',
    which in turn rely on 'converted_states1'. This fact shows that we can
    tie the knot by making a promise to compute a state, add this promise
    to the dictionary of the built states, and use the updated dictionary
    to build the descendants. Because Haskell is a non-strict language, we
    don't need to do anything special to make the promise. Every
    computation is Haskell is by default a promise.

    We can print the DFA for dom18 to see what we've got:

    CCardFA> finAuDeterminize dom18
    CCardFA>-- which shows
    CCardFA> [{@{State [1]   True  [[1,2]] [[]]   }
    CCardFA>    {State [1,2] True  [[1,2]] [[1,2]]}
    CCardFA>    {State []    False [[]]    [[]]   }@}] 

    which is indeed a DFA (which happens to be minimal)
    recognizing (0+1)* - 1(0+1)*

    We can run the determinized FA using the same function finAuAcceptStringQ:

    > test1' = finAuAcceptStringQ (finAuDeterminize dom18) $ map (>0) [0,1,0,1]
    > test2' = finAuAcceptStringQ (finAuDeterminize dom18) $ map (>0) [1,1,0,1]

    Another example:

    > dom19 = [one,two]
    >     where one = FaState 1 True [two] []
    > 	    two = FaState 2 True [one] [one]

    CCardFA> finAuDeterminize dom19
    CCardFA> -- shows
    CCardFA> [{@{State [1,2] True  [[1,2]] [[1]] }
    CCardFA>    {State [1]   True  [[2]]   [[]]  }
    CCardFA>    {State [2]   True  [[1]]   [[1]] }
    CCardFA>    {State []    False [[]]    [[]]  }@}] 

    which recognizes (0+1)* - (0+1)*11(0+1)*

    Finally, here's an example with a 'diamond' cycle

    > dom20 = [zero]
    >   where zero = FaState 0 False [one]   [two]
    >         one  = FaState 1 False [three] [four]
    >         two  = FaState 2 False [four,five] [four]
    >         three= FaState 3 True  []  []
    >         four = FaState 4 True  [two]  [two]
    >         five = FaState 5 True  [] []

    CCardFA> print dom20
    CCardFA> print $ finAuDeterminize dom20

    Another example of tying a knot in the case of _forward links_, by
    using a fixed-point combinator, is discussed in 
        http://www.mail-archive.com/haskell@haskell.org/msg10687.html

-}


{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}

{-

module Control.Monad.Oracle
       ( OracleT
       , evalOracleT
       , execOracleT
       , runOracleT
       ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.RWS

newtype OracleT o s m a = OracleT (RWST o o s m a) deriving
  ( Alternative
  , Applicative
  , Functor
  , Monad
  , MonadCont
  , MonadError e
  , MonadFix
  , MonadIO
  , MonadPlus
  , MonadReader o
  , MonadWriter o
  , MonadState s
  , MonadRWS o o s
  , MonadTrans )

evalOracleT :: (Functor m, MonadFix m) => OracleT o s m a -> s -> m (a, o)
evalOracleT m s =
  let discardSnd (a, _, c) = (a, c)
  in discardSnd <$> runOracleT m s

execOracleT :: (Functor m, MonadFix m) => OracleT o s m a -> s -> m (s, o)
execOracleT m s =
  let discardFst (_, b, c) = (b, c)
  in discardFst <$> runOracleT m s

runOracleT :: MonadFix m => OracleT o s m a -> s -> m (a, s, o)
runOracleT (OracleT rwst) s = do
  rec (a, s', o) <- runRWST rwst o s
  return (a, s', o)

type Knot k s a = RWS k k s a

tie :: Knot k s a -> s -> (a, s, k)
tie knot s0 =
  let (a, s1, k) = runRWS knot k s0 -- this is fucking interesting
  in (a, s1, k)

tie :: Knot k s a -> s -> (a, s, k)
tie knot s =
  fix $ \ ~(_, _, k) -> runRWS knot k s -- this is severely fucking interesting

data Node = Node {
  value :: Int,
  next  :: Node
} deriving Show

let n0 = Node { value = 0, next = n1 }
    n1 = Node { value = 1, next = n2 }
    n2 = Node { value = 2, next = n0 }

data ParserState = ParserState {
  current   :: Int,
  remaining :: [(Int, Int)]
}

type NodeParser a = Knot (Map Int Node) ParserState a

parse :: NodeParser ()
parse = do
  s <- get
  case remaining s of
    [] -> return ()
    ((val, ref) : rest) -> do
      m <- ask
      tell $ Map.singleton (current s) (Node val $ m ! ref)
      put  $ ParserState (current s + 1) rest
      parse

example :: Node
example =
  let (_, _, m) = tie parse $ ParserState 0 [(0, 1), (1, 2), (2, 0)]
  in (m Map.! 0)

main :: IO ()
main = putStrLn $ show example

-}