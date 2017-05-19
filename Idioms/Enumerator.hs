-- Enumerator and iteratee
-- ref: https://wiki.haskell.org/Enumerator_and_iteratee
-- okmij: http://okmij.org/ftp/Haskell/Iteratee/describe.pdf

-- An enumerator is something that knows how to generate a list 
-- An iteratee is something that does one step in processing another piece of the big list.

-- foldl (+) 0 xs
-- foldl : enumerator
-- ((+), 0) iteratee

-- Separation of concerns
    -- fold(enumerator) has the intimate knowledge of the collection and how to get to the next element
    -- iteratee knows what to do with the current element
        -- (+) not to care which collection the element from
        -- 0 unware of the collection

-- F-algebra
    -- ((+), 0) is an F-algebra
    -- foldl (+) 0 is a catamorphism

-- iteratee is an automaton
    -- From this point of view, the enumerator sends elements of a list sequentially, from head to tail, as input messages to the iteratee. If the iteratee finishes, it outputs an accumulator. If the iteratee continues, it outputs nothing (i.e., ()).

-- a set of states of iteratee is divided into subsets "Done" and "Next". 
    -- Done-state means that automaton finished consuming a list, i.e., the automaton is dead. 
    -- Next-state means that you can give an input message and obtain the same automaton in a new state.

data Iteratee i o = Done o | Next (i -> Iteratee i o)

    -- i is the type of the iteratee's input messages (or list elements)-- o is a type of the output message (an accumulator).
    -- Iteratee stores not an automaton, but an automaton in some state, an automaton with distinguished state.

-- The distinct feature of iteratee is that it can say after which list element an iteratee finishes. An iteratee says this by sending "Done" to an enumerator. Then the enumerator can, for example, close a file or a socket (a stream) where a list of characters is read from. Lazy I/O, which uses lazy lists, closes a stream only when the stream is exhausted.

-- The drawback is that an enumerator can not tell an iteratee that an input is exhausted — an Iteratee consumes only infinite lists. You can remedy this by assuming
    -- i == Maybe i' (That's just fix)

-- sample enumerator that takes input messages from a file
enumerator :: FilePath -> Iteratee (Maybe Char) o -> IO o
enumerator file it = withFile file ReadMode
  $ \h -> fix (\rc it -> case it of
    Done o -> return o
    Next f -> do
      eof <- hIsEOF h
      case eof of
        False -> do
          c <- hGetChar h
          rc (f (Just c))
        True -> rc (f Nothing)
    ) it

-- 2. Functions

-- pi-calculus notation

-- Monadic parsing (it0 -> it1)

-- You can compose iteratees sequentially in time. This is done by (>>). it0 >> it1 means that when it0 finishes, it1 starts. Generally speaking, Iteratee i is a Monad, and it works exactly like a monadic parser.

{- s = state -}
instance Functor (Iteratee input) where
  fmap f = fix $ \rc s -> case s of
    Done o -> Done (f o)
    Next g -> Next (rc . g)

instance Monad (Iteratee input) where
  return = Done
  it0 >>= it1 = fix (\rc s -> case s of
    Done o -> it1 o
    Next g -> Next (rc . g)
    ) it0

-- Repetitive parsing (it1 | !it0)

-- You can also compose iteratees sequentially in space. it0's output messages become it1's input messages, so it0 and it1 work in parallel. Their composition is denoted it1 . it0. If it0 finishes, it is resurrected to its original state. If it1 finishes, it1 . it0 finishes — The main feature here is that it0 is restarted, as this is used for repetitive parsing.

arr0 f = Next $ \i -> Done (f i)
instance Category Iteratee where
  id = arr0 id
  it1 . it0 = fix (\rc1 it1 -> case it1 of
    Done c -> Done c
    Next f1 -> fix (\rc0 it0 -> case it0 of
      Done b -> rc1 (f1 b)
      Next f0 -> Next (rc0 . f0)
      ) it0
    ) it1

-- 3. Generalization

-- You may note that Iteratee is a final coalgebra. Other kinds of automata can be described with other F-coalgebras. In practice such automata can handle network protocols or interactive user input. See for example papers by Bart Jacobs for theoretical discussion.

{-

    type Iteratee el m a −− a processor of the stream of els
                         −− in a monad m yielding the result of type a
    instance Monad m ⇒ Monad (Iteratee el m)
    instance MonadTrans (Iteratee el )
    
    getchar :: Monad m ⇒ Iteratee el m (Maybe el) −− cf. IO.getChar, List . head
    
    count_i :: Monad m ⇒ Iteratee el m Int −− cf. List . length
    
    run :: Monad m ⇒ Iteratee el m a → m a −− extract Iteratee ’ s result
    
    −− A producer of the stream of els in a monad m
    
    type Enumerator el m a = Iteratee el m a → m (Iteratee el m a)
    
    enum file :: FilePath → Enumerator Char IO a −− Enumerator of a file
    
    −− A transformer of the stream of elo to the stream of eli
    −− (a producer of the stream eli and a consumer of the stream elo )
    type Enumeratee elo eli m a =
        Iteratee eli m a → Iteratee elo m (Iteratee eli m a)
    
    en_filter :: Monad m ⇒ (el → Bool) → Enumeratee el el m a
    
    take :: Monad m ⇒ Int → Enumeratee el el m a −− cf. List . take
    
    enum_words :: Monad m ⇒ Enumeratee Char String m a −− cf. List.words
    
    −− Kleisli (monadic function) composition: composing enumerators
    (>>>) :: Monad m ⇒ (a → m b) → (b → m c) → (a → m c)
    
    −− Connecting producers with transformers (cf . (= ))
    infixr 1 . | −− right−associative
    (.|) :: Monad m ⇒
    (Iteratee el m a → w) → Iteratee el m (Iteratee el ’ m a) → w
    
    −− Parallel composition of iteratees (cf . List . zip )
    en_pair :: Monad m ⇒
    Iteratee el m a → Iteratee el m b → Iteratee el m (a,b)

-}