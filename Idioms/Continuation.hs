-- Continuation
-- ref: https://wiki.haskell.org/Continuation
-- okmij: http://okmij.org/ftp/continuations/index.html
-- ref: http://www.haskellforall.com/2012/12/the-continuation-monad.html
-- wiki: https://en.wikipedia.org/wiki/Delimited_continuation

-- 1. General or introductory materials

    -- 1.1 metaphors

        -- 1.1.1 imperative

            -- representation of the execution state of a program (eg. callstack) at a certain point in time

            -- call/cc is something like the goto instruction, but a Grand High Exalted goto instruction
                -- the point of call/cc is that it is not a static (lexical) goto but a dynamic one
                -- ref: http://www.madore.org/~david/computers/callcc.html#sec_intro

        -- 1.1.2 functional

            -- representation of the future of a computation, as a function from an intermediate result to the final result

            -- The idea behind CPS is to pass around as a function argument what to do next
                -- YAHT: https://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Type_basics#Continuation_Passing_Style
            
            -- Rather than return the result of a function, pass one or more Higher Order Functions to determine what to do with the result. Yes, direct sum like things (or in generally, case analysis, managing cases, alternatives) can be implemented in CPS by passing more continuations.

-- 2. Examples

id :: a -> a
id a = a

idCPS :: a -> (a -> r) -> r
idCPS a ret = ret a

 fac :: Integral a => a -> a
 fac 0 = 1
 fac n'@(n + 1) = n' * fac n

 facCPS :: a -> (a -> r) -> r
 facCPS 0 k = k 1
 facCPS n'@(n + 1) k = facCPS n $ \ret -> k (n' * ret)
 
--  Any function calling a CPS-ed function must either provide a new continuation or pass its own; any calls from a CPS-ed function to a non-CPS function will use implicit continuations. Thus, to ensure the total absence of a function stack, the entire program must be in CPS.
-- Operations considered primitive for CPS tend to be arithmetic, constructors, accessors, or mutators; any O(1) operation will be considered primitive.

-- The function Foreign.C.String.withCString converts a Haskell string to a C string.
-- But it does not provide it for external use but restricts the use of the C string to a sub-procedure, because it will cleanup the C string after its use.
-- It has signature withCString :: String -> (CString -> IO a) -> IO a.
-- This looks like continuation and the functions from continuation monad can be used, e.g. for allocation of a whole array of pointers:

-- However, the right associativity of mapM leads to inefficiencies here.
multiCont :: [(r -> a) -> a] -> ([r] -> a) -> a
multiCont xs = runCont (mapM Cont xs)
 
withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 strings act =
   multiCont
      (map withCString strings)
      (\rs -> withArray0 nullPtr rs act)

-- the type of the (non-continuation) argument of the discussed functions (idCPS, mysqrtCPS, facCPS)
-- the type of the argument of the continuations
-- can be different

mylength :: [a] -> Integer
mylength [] = 0
mylength (_ : as) = succ (mylength as)

mylengthCPS :: [a] -> (Integer -> r) -> r
mylengthCPS [] k = k 0
mylengthCPS (_ : as) k = mylengthCPS as (k . succ)

-- 3. Continuation Monad
    -- Control.Monad.Cont

-- 4. Delimited continuation
   
-- CC-delcont
-- ref: https://wiki.haskell.org/Library/CC-delcont

-- 4.1 undelimited

    -- A continuation of an expression is, in a loose sense, 'the stuff that happens after the expression.' An example to refer to may help:

    -- m >>= f >>= g >>= h

    -- ordinary monadic pipeline

    -- Here we have an ordinary monadic pipeline. A computation m is run, and its result is fed into f, and so on. We might ask what the continuation of 'm' is, the portion of the program that executes after m, and it looks something like:

    -- \x -> f x >>= g >>= h

    -- The continuation takes the value produced by m, and feeds it into 'the rest of the program.' But, the fact that we can represent this using functions as above should be a clue that continuations can be built up using them, and indeed, this is the case. There is a standard way to transform a program written normally (or in a monadic style, as above) into a program in which continuations, represented as functions, are passed around explicitly (known as the CPS transform), and this is what Cont/ContT does.

    -- callCC (\k -> e) >>= f >>= g >>= h

    -- 'k' will be set to a function that is something like the above '\x -> f x >>= g >>= h'. However, in some sense, it is not an ordinary function, as it will never return to the point where it is invoked. Instead, calling 'k' should be viewed as execution jumping to the point where callCC was invoked, with the entire 'callCC (..)' expression replaced with the value passed to 'k'. So k is not merely a normal function, but a way of feeding a value into into an execution context (and this is reflected in its monadic type: a -> Cont b).

-- computing the product of a list of numbers:
-- when find 0, directly return 0
prod l = callCC (\k -> loop k l) where
    loop _ []     = return 1
    loop k (0:_)  = k 0
    loop k (x:xs) = do n <- loop k xs ; return (n*x)


-- 4.2 delimited continuatons

-- interactive environments in which code can be defined and run (like GHCi)
-- Naturally, it would be nice if such environments could themselves be written in Scheme.

-- In Haskell, continuation using code is tagged with a monadic type, and one must use runCont(T) to run such computations, and the effects can't escape it. 
-- In Scheme, continuations are native, and all code can capture them, and capturing them captures not 'the rest of the Cont(T) computation,' but 'the rest of the program.' 

-- If the interactive loop is written in Scheme, this includes the loop itself, so programs run within the session can affect the session itself.

-- The idea was, to tag a point at which the interactive loop invoked some sub-program, and then control flow operators such as callCC would only be able to capture a portion of the program up to the marker. To the sub-program, this is all that's of interest anyhow.

-- 4.3 Example

-- 4.3.1 Iterator
data Tree a = Leaf | Branch a (Tree a) (Tree a)
 
empty = Leaf
singleton a = Branch a Leaf Leaf
 
insert b Leaf = Branch b Leaf Leaf
insert b (Branch a l r)
    | b < a = Branch a (insert b l) r
    | otherwise = Branch a l (insert b r)
 
fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold f z Leaf = z
fold f z (Branch a l r) = f a (fold f z l) (fold f z r)
 
for :: Monad m => Tree a -> (a -> m b) -> m ()
for t f = fold (\a l r -> l >> f a >> r) (return ()) t

data Iterator m a = Done | Cur a (m (Iterator m a))
 
begin :: MonadDelimitedCont p s m => Tree a -> m (Iterator m a)
begin t = reset $ \p ->
            for t (\a ->
                shift p (\k -> return (Cur a (k $ return ())))) >> return Done
 
current :: Iterator m a -> Maybe a
current Done      = Nothing
current (Cur a _) = Just a
 
next :: Monad m => Iterator m a -> m (Iterator m a)
next Done      = return Done
next (Cur _ i) = i
 
finished :: Iterator m a -> Bool
finished Done = True
finished _    = False

-- There are two delimited control operators in play here. First is reset, which is a way to place a delimiter around a computation. The term 'p' is simply a way to reference that delimiter; the library I'm working with allows for many named delimiters to exist, and for control operators to specify which delimiters they're working with (so a control operator may capture the continuation up to p, even if it runs into a delimiter q sooner, provided p /= q).

-- The other operator is shift, which is used to capture the delimited continuation. In many ways, it's like callCC, but with an important difference: it aborts the captured continuation. When callCC is called on a function f, if f returns normally, execution will pick up from just after the callCC. However, when shift is called, the continuation between the call and the enclosing prompt is packaged up (into 'k' here), and passed to the function, and a normal return will return to the place where the delimiter was set, not where shift was called.

-- With this in mind, we can begin to analyze the 'begin' function. First, it delimits a computation with the delimiter 'p'. Next, it begins to loop over the tree. For each element, we use 'shift' to capture "the rest of the loop", calling it 'k'. We then package that, and the current tree element, into an Iterator object, and return it. Since the shift has aborted the rest of the loop (for the time being), it returns to where 'reset' was called, and the function returns the iterator object (wrapped in a monad, of course).

-- The main remaining piece of interest is when next goes to get the next element of the traversal. When this happens, 'k $ return ()' is executed, which invokes the captured continuation (with the value (), because the loop doesn't take the return value of the traversal function into account anyway). This, essentially, re-enters the loop. If there is a next element, then the traversal function is called with it, shift will once again capture 'the rest of the loop' (from a later point that before, though), and return an iterator object with the new current element and continuation. If there are no new elements, then control will pass out of the loop to the following computation, which is, in this case, 'return Done', so in either case, an Iterator object is the result, and the types work out.

{-
main :: IO ()
main = runCCT $ do t <- randomTree 10
                   i <- begin t
                   doStuff i
 where
 doStuff i
    | finished i = return ()
    | otherwise  = do i'  <- next i
                      i'' <- next i
                      liftIO $ print (fromJust $ current i :: Int)
                      doStuff i'
 
randomTree n = rt empty n
 where
 rt t 0 = return t
 rt t n = do r <- liftIO randomIO
             rt (insert r t) (n - 1)
-}

-- 4.3.2 BFS

data Tree a = Node a (Tree a) (Tree a) | Leaf a
 
t = Node 1 (Node 2 (Leaf 3)
                   (Node 4  (Leaf 5)
                            (Leaf 6)))
           (Node 7 (Node 8  (Leaf 9)
                            (Leaf 10))
                   (Leaf 11))
 
toList (Leaf i) = [i]
toList (Node a t1 t2) = a : toList t1 ++ toList t2

visit :: MonadDelimitedCont p s m => p [a] -> Tree a -> m ()
visit p = visit'
 where
 visit' (Leaf i)       = control p $ \k -> (i:) `liftM` k (return ())
 visit' (Node i t1 t2) = control p $ \k -> do a <- k (return ())
                                              visit' t2
                                              visit' t1
                                              (i:) `liftM` return a
 
bf :: MonadDelimitedCont p s m => Tree a -> m [a]
bf t = reset $ \p -> visit p t >> return []

{-

As the slides say, the key idea is to "return before recursively traversing the subtrees." This is accomplished through the use of the delimited control operator 'control.' At the Node stage of a traversal, control is used to capture the sub-continuation that comes after said Node (which is, effectively, the traversal over the rest of the nodes at the same level). However, instead of descending depth-first style, that sub-continuation is immediately invoked, the result being called a. Only after that are the sub-trees descended into.

It should be noted, also, that this particular example can be used to display a difference between 'shift' (the so-called 'static' delimited operator) and 'control' (which is one of the 'dynamic' operators). The difference between the two is that in 'shift p (\k -> e)' calls to k are delimited by the prompt p, whereas in control, they are not (in both, e is). This has important consequences. For instance, at some point in a traversal an evaluation may look something like:

delimited (visit' t2 >> visit' t1)
Which, using some simplified notation/traversal, expands to:

delimited (control (\k -> k () >> visit' t22 >> visit' t21)
  >> control (\k -> k () >> visit' t12 >> visit' t11))
Which, due to the effects of control turns into:

delimited ((control (\k -> k () >> visit' t12 >> visit' t11)) >> visit' t22 >> visit' t21)
 
 ==>
 
delimited (visit' t22 >> visit' t21 >> visit' t12 >> visit' t11)
In other words, using 'control' ends up building and executing a sequence of traversals at the same level, after the actions for the above level performed by the 'k ()'. The control operators of the lower level are then free to close over, and manipulate all the visitations on there level. This is why the result is a breadth-first traversal. However, replacing control with shift, we get:

delmited (visit' t2 >> visit' t1)
 
  ==>
 
delimited ((shift (\k -> k () >> visit' t22 >> visit' t21))
  >> (shift (\k -> k () >> visit' t12 >> visit' t11)))
 
  ==>
 
delimited (delimited (shift (\k -> k () >> visit' t12 >> visit' t11)) >> visit' t22 >> visit' t21)
And already we can see a difference. The sub-traversal of t1 is now isolated, and control effects (via shift, at least) therein cannot affect the sub-traversal of t2. So, control effects no longer affect an entire level of the whole tree, and instead are localized to a given node and its descendants. In such a case, we end up with an ordinary depth-first traversal (although the sub-continuations allow the visitation of each node to look a bit different than toList, and since we're always pre-pending, as we get to a node, the results are reversed compared to toList).

In any case, the desired result has been achieved: A slightly modified recursive descent traversal has allowed us to express breadth-first search (and depth-first search in the same style is a matter of substitution of control operators) without having to do the normal list-of-sub-trees sort of bookkeeping (although the added weight of working with delimited control may more than outweigh that).

For a more in-depth discussion of the differences between shift, control and other, similar operators, see Shift to Control, cited below.

-}

-- 4.3.3 Resumable Parsing

{-

The problem is similar to the above iterator example. Specifically, we are in need of a parser that can take fragments of input at a time, suspending for more input after each fragment, until such time as it can be provided. However, there are already plenty of fine parsing libraries available, and ideally, we don't want to have to re-write a new library from scratch just to have this resumable parser feature.

As it turns out, delimited continuations provide a fairly straightforward way to have our cake and eat it too in this case. First, we'll need a data type for the resumable parser.

-- Needs Rank2Types
data Request m a = Done a | ReqChar (Maybe Char -> m (Request m a))
Such a parser is either complete, or in a state of requesting more characters. Again, we'll have some convenience functions for working on the data type:

provide :: Monad m => Char -> Request m a -> m (Request m a)
provide _ d@(Done _)  = return d
provide c (ReqChar k) = k (Just c)
 
provideString :: Monad m => String -> Request m a -> m (Request m a)
provideString []     s = return s
provideString (x:xs) s = provide x s >>= provideString xs
 
finish :: Monad m => Request m a -> m (Request m a)
finish d@(Done _)  = return d
finish (ReqChar k) = k Nothing
So, 'provide' feeds a character into a parser, 'provideString' feeds in a string, and 'finish' informs the parser that there are no more characters to be had.

Finally, we need to have some way of suspending parsing and waiting for characters. This is exactly what delimited continuations do for us. The hook we'll use to get control over the parser is through the character stream it takes as input:

toList :: Monad m => m (Maybe a) -> m [a]
toList gen = gen >>= maybe (return []) (\c -> liftM (c:) $ toList gen)
 
streamInvert :: MonadDelimitedCont p s m => p (Request m a) -> m (Maybe Char)
streamInvert p = shift p (\k -> return $ ReqChar (k . return))
 
invertParse :: MonadDelimitedCont p s m => (String -> a) -> m (Request m a)
invertParse parser = reset $ \p -> (Done . parser) `liftM` toList (streamInvert p)
So, 'toList' simply takes a monadic action that may produce a character, and uses it to produce a list of characters (stopping when it sees a 'Nothing'). 'streamInvert' is just such a monadic, character-producing action (given a delimiter). Each time it is run, it captures a sub-continuation (here, 'the rest of the list generation'), and puts it in a Request object. We can then pass around the Request object, and feed characters in as desired (via 'provide' and 'provideString' above), gradually building the list of characters to be parsed.

In the 'invertParse' method, this gradually produced list is fed through a parser (of type String -> a, so it doesn't need to know about the delimited continuation monad we're using), and the output of the parser is packaged in a finished (Done) Request object, so when we finally call 'finish', we will be able to access the results of the parser.

For this example, the words function suffices as a parser:

gradualParse :: [String]
gradualParse = runCC $ do p1 <- invertParse words
                          p2 <- provideString "The quick" p1
                          p3 <- provideString " brown fox jum" p2
                          p4 <- provideString "ps over the laz" p3
                          p5 <- provideString "y dog" p4 >>= finish
                          p6 <- provideString "iest dog" p4 >>= finish
                          let (Done l1) = p5
                              (Done l2) = p6
                          return (l1 ++ l2)
 
main :: IO ()
main = mapM_ putStrLn gradualParse

It will pause at arbitrary places in the parse, even in the middle of tokens, and wait for more input. And one can resume a parse from any point to which a Request pointer is saved without interfering with other resumable parser objects

(A note: depending on what exactly one wants to do with such parsers, there are a few nits in the above implementation. It doesn't exactly match the semantics of the OCaml parser. For more information on this topic, see the linked mailing-list thread, as it discusses the issues, their causes, and provides an alternate implementation (which changes mostly the parser, not the delimited continuation end) which matches the OCaml version much more closely)

-}


-- 5. Linguistics
    -- 

-- 6. Applicative
    -- ZipperFS

