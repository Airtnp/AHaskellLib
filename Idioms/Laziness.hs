-- Laziness
-- ref: https://wiki.haskell.org/Maintaining_laziness
-- ref: https://wiki.haskell.org/Laziness_is_not_always_good

-- Maintaining laziness

-- However many Haskell libraries found on Hackage are implemented as if Haskell were a strict language. This leads to unnecessary inefficiencies, memory leaks and, we suspect, unintended semantics.

-- Checking laziness

-- feed it by undefined
    -- undefined
    -- error
    -- infinite loop

testFilter0 = filter even [0..100] `isPrefixOf` filter even [0..]
testFilter1 = filter even [0..100] `isPrefixOf` filter even ([0..102]++undefined)
testFilter2 = let x = filter even [0..] !! 100 in x==x
testFilter3 = let x = filter even ([0..102]++undefined) !! 50 in x==x

-- Or module StrictCheck

{-

    *StrictCheck> test1 10 (unzip :: [(Int,Int)] -> ([Int],[Int]))
    Function seems not to be least strict.
    Input(s): _|_
    Current output: _|_
    Proposed output: (_|_, _|_)
    Continue?

-}

-- Break laziness

-- Maybe/Either/Exceptions

decodeUTF8 :: [Word8] -> Either Message String

-- The Either type signals that the function marks decoding-failure by using the Left constructor of Either. This function cannot be lazy, because when you access the first character of the result, it must already be computed, whether the result is Left or Right.

-- For this decision, the complete input must be decoded. A better type signature is

decodeUTF8 :: [Word8] -> (Maybe Message, String)

-- where the String contains as much characters as could be decoded and Maybe Message gives the reason for the stop of the decoding. Nothing means the input was completely read, Just msg means the decoding was aborted for the reason described in msg.

-- If you touch the first element of the pair, the complete decodings is triggered, thus laziness is broken.

-- Instead of the unspecific pair type you should use the special type for asynchronous exceptions as found in the explicit exception package.

-- Especially in parsers you may find a function, called Wadler's force function. It works as follows:

force y =
   let Just x = y
   in  Just x

-- It looks like a complicated expression for y with an added danger of failing unrecoverably when y is not Just. Its purpose is to use the lazy pattern matching of let and to show to the runtime system, that we expect that y is always a Just.

{-

    Then the runtime system does not need to wait until it can determine the right constructor but it can proceed immediately.

    This way, a function can be made lazy, also if it returns Maybe. It can however fail, if later it turns out, that y is actually Nothing.
    Using force-like functions is sometimes necessary, but should be avoided for data types with more than one constructor. It is better to use an interim data type with one constructor and lift to the multi-constructor datatype when needed.

    Consider parsers of type StateT [Word8] Maybe a. Now consider the parser combinator
    many :: StateT [Word8] Maybe a -> StateT [Word8] Maybe [a]
    which parses as many elements of type a as possible. It shall be lazy and thus must be infallible and must not use the Maybe.
    It shall just return an empty list, if parsing of one element fails.

    A quick hack would be to define many using a force function. It would be better to show by the type, that many cannot fail:
    many :: StateT [Word8] Maybe a -> StateT [Word8] Identity [a]

-}

-- Early decision

-- List construction

-- Be aware that the following two expressions are not equivalent.

-- less lazy
if b then f x else f y
-- more lazy
f (if b then x else y)

-- It is if undefined then f x else f y is undefined, 
-- whereas f (if b then x else y) is f undefined, 
-- which is a difference in non-strict semantics. Consider e.g. if b then 'a':x else 'a':y.

-- It is common source of too much strictness to make decisions too early and thus duplicate code in the decision branches. Intuitively spoken, the bad thing about code duplication (stylistic questions put aside) is, that the run-time system cannot see that in the branches, some things are equal and do it in common before the critical decision. Actually, the compiler and run-time system could be "improved" to do so, but in order to keep things predictable, they do not do so. Even more, this behaviour is required by theory, since by pushing decisions to the inner of an expression you change the semantics of the expression. So we return to the question, what the programmer actually wants.

if b
  then [x]
  else y:ys

-- = null (if undefined then [x] else y:ys) ===> undefined

let z:zs =
      if b
        then [x]
        else y:ys
in  z:zs

-- = ===> False

-- This expression always returns the constructor (:) and thus null knows that the list is not empty. However, this is a little bit unsafe, because the let z:zs may fail if in the branches of if there is an empty list.

let (z,zs) =
      if b
        then (x,[])
        else (y,ys)
in  z:zs

-- = uncurry (:) (if b then (x,[]) else (y,ys))

-- inits

inits        :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [[]] ++ map (x:) (inits xs)

-- inits undefined = undefined

inits :: [a] -> [[a]]
inits xt =
   [] :
   case xt of
      [] -> []
      x:xs -> map (x:) (inits xs)

-- Reader-Writer-State Monad

-- Consider the following action of the Control.Monad.RWS which fetches a certain number of elements from a list. The state of the monad is the input list we fetch the elements from. The reader part provides an element which means that the input is consumed. It is returned as singleton when the caller tries to read from a completely read input. The writer allows to log some information, however the considered action does not output anything to the log.

getN :: Int -> RWS a [Int] [a] [a]
getN n =
   do input <- get
      if null input
        then asks (:[])
        else let (fetched,rest) = splitAt n input
             in  put rest >> return fetched

-- Although getN does obviously not log something (i.e. it does not call tell), it requires to read the input in order to find out, that nothing was logged:

-- runRWS (getN 5) '\n' undefined === undefined

-- The problem is again, that if checks the emptiness of the input, which is undefined, since the input is undefined.

-- We start refactoring by calling put independently from input's content. It works as well for empty lists, since splitAt will just return empty lists in this case.

getN :: Int -> RWS a [Int] [a] [a]
getN n =
   do input <- get
      let (fetched,rest) = splitAt n input
      put rest
      if null input
        then asks (:[])
        else return fetched

-- This doesn't resolve the problem. There is still a choice between asks and return. We have to pull out ask as well.

getN :: Int -> RWS a [Int] [a] [a]
getN n =
   do input <- get
      let (fetched,rest) = splitAt n input
      put rest
      endOfInput <- ask
      return $
         if null input
           then [endOfInput]
           else fetched

-- runRWS (getN 5) '\n' undefined = []

-- We learn from this example, that sometimes in Haskell it is more efficient to call functions that are not needed under some circumstances. Always remind, that the do notation looks only imperative, but it is not imperative.

-- Strict pattern matching in a recursion

-- Consider the partition function which sorts elements, that match a predicate, into one list and the non-matching elements into another list. This function should also work on infinite lists, but the implementation shipped with GHC up to 6.2 failed on infinite lists. What happened?

-- The reason was that pattern matching was too strict.

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p =
   foldr
      (\x ~(y,z) ->
         if p x
           then (x : y, z)
           else (y, x : z))
      ([],[])

-- The usage of foldr seems to be reserved for advanced programmers. Formally foldr runs from the end to the start of the list.

-- for infinite list

-- partition p (a:as) =
   -- (\ ~(y,z) -> if p a then (a:y, z) else (y, a:z)) (foldr ... ([],[]) as)

-- We see that the whether a is prepended to the first or the second list, does only depend on p a, and neither on y nor on z. The laziness annotation ~ is crucial, since it tells, intuitively spoken, that we can rely on the recursive call of foldr to return a pair and not undefined.

-- Omitting it, would require the evaluation of the whole input list before the first output element can be determined.

-- This fails for infinite lists and is inefficient for finite lists, and that was the bug in former implementations of partition.

-- Btw. by the expansion you also see, that it would not help to omit the tilde and apply the above 'force' trick to the 'if-then-else' expression.

-- There is an unnecessary decision between the pair constructor of the initial accumulator value ([],[]) and the pair constructors within the if. This can only be avoided by applying a force function to the result of foldr:

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p =
   (\ ~(ys,zs) -> (ys,zs)) .
   foldr
      (\x ~(y,z) ->
         if p x
           then (x : y, z)
           else (y, x : z))
      ([],[])

-- List reversal

-- Any use of the list function reverse should alert you, since when you access the first element of a reversed list, then all nodes of the input list must be evaluated and stored in memory.


-- Input and Output

{-

    In general functions, output of lazily generated data is no problem, whereas lazily reading data requires a sort of a hack and thus caution. Consider the nice program

    readFile "source" >>= writeFile "target"

    which copies the file source to the file target with constant memory consumption, since readFile reads the data lazily and writeFile writes it as it comes in.

    However it fails badly, when a file shall be updated in-place:

    readFile "text" >>= writeFile "text" . map toUpper

    This would work only when readFile would be strict, that is it would read the file contents to memory before returning. The function readFile needs certain hacks:

        The function unsafeInterleaveIO is needed for deferring the calls to hGetChar until the characters are actually needed.

        Exceptions, that occur while reading the file, are raised in the code that writes the result of processing the file content to somewhere. I.e. the exceptions produced by readFile can occur in code that has nothing to do with file reading and there is no warning, that they might occur there. Again, I want to advertise the explicit exception package, which helps making the reason for the stop of the file read explicit. Exceptions must still be handled in code, that does not read the file, but the fact that they are explicit helps you to not forget it.

        The file must be closed after it is no longer needed. The documentation says, that the file is put into a semi-closed state. Maybe this means, it uses Weak Reference which lets the garbage collector close the file, once no reference to data of the file exists anymore. However, the garbage collector never works immediately, but in phases. It may be that the file remains open for a long time, maybe until the program exits. The Data.ByteString.Lazy.readFile function explicitly closes the file after the last byte is read. The advantage is, that the file is closed immediately. The disadvantage is, that the file is not closed at all, when not all bytes are read. E.g. if a parser encounters a parse error, it has to read the rest of the file anyway, in order to get it closed.

    A function that handles the closing of the file for you is System.IO.withFile.

    You can use it like

    withFile "source" ReadMode $ \h ->
    hGetLine h >>= putStrLn

    After the actions inside the withFile call, the file is closed.
    However this is dangerous:

    If you leak lazily read contents from the file out of withFile, the file is closed before the data is actually read. Thus, although withFile "source" ReadMode hGetContents looks like readFile,
    it is very different: I does not work.

    How can you implement a function like hGetContents by yourselves? You need to call hGetChar in a lazy way. This is achieved by unsafeInterleaveIO. However, calling unsafeInterleaveIO hGetChar many times would not work, because the order must be preserved.

    E.g. in

    hGetContents h >>= putStrLn . drop 10
    
    , the first ten characters from the file are not needed, but hGetChar must be called for the first 10 characters anyway in order to increment the file position. This is achieved by not calling unsafeInterleaveIO on hGetChar but on the list constructor. The implementation of hGetContents looks roughly like

    hGetContents h =
    let go = unsafeInterleaveIO $ liftM2 (:) (hGetChar h) go
    in  go
    .

    In contrast to the standard hGetContents, this implementation does not close the file
    (by the way, it does even not handle the end of the file), but the advantage of not relying on some automatism to close the file somewhen is, that you can close the file immediately after you stopped processing its content. The disadvantage is that you must not forget to close the file and must do it only once.

    So far we have only considered lazy read. It might also be necessary to trigger write actions when fetching data. Consider a server-client interaction, where data can only be read, when a request was sent before. It would be nice if the request is triggered by reading the result from the server. Such interactions can be programmed using the lazyio package.

-}

-- Laziness is not always good

{-

    Consider the Monoid instance of the null type ():
    mempty = ()
    mappend _ _ = ()
    These functions are least strict, but have a subtle problem: They do not generally satisfy the monoid laws.

    Remind you: mempty must be the identity element with respect to mappend:
    forall a. mappend mempty a = a
    forall a. mappend a mempty = a
    You find that it is not mappend mempty undefined = undefined, but mappend mempty undefined = mempty.

-}

-- The solution of this issue is to define

mempty = ()
mappend () () = ()
 
force :: () -> ()
force _ = ()

mappend (force a) (force b) -- instead of mappend a b

