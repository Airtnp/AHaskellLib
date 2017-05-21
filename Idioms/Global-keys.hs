-- Global keys
-- ref: https://wiki.haskell.org/Global_keys
-- proposal: https://wiki.haskell.org/Top_level_mutable_state


-- strange unnatural discrepancy between the fixed set of built-in privileged operations such as Data.Unique.newUnique which are "allowed" to make use of global state, and user defined operations which have to rely on a shaky hack in order to preserve natural abstraction barriers between components such as a user-defined Unique, Atom, and anything involving memoisation or device management etc.

-- The kind of applications we have in mind (please add more) are:

-- A source of random numbers, or of unique numbers. This should be on a per-thread basis.

-- The value of 'stdin' or 'stdout'. We don't want to mutate this (although note that the handle itself contains mutable state), but we might want to set the value for sub-computations, including any spawned threads.

-- A straw man

-- The basic idea is
    -- Each thread has access (via the IO or STM monad) to a "dictionary" that maps a (typed) key to a value of that type.
    -- The dictionary is not mutable, but the values might themselves be mutable cells (think of 'stdin').

-- More concretely
    -- A new built-in data type, Key a, an instance of Eq, but not Ord. It's a "thing with identity" (TWI).
    -- A new built-in data type of dictionaries, Dict, which maps a typed key to a typed value:

lookup :: Dict -> Key a -> Maybe a
insert :: Dict -> Key a -> a -> Dict
union  :: Dict -> Dict -> Dict
-- etc

-- You can allocate a fresh (globally unique) key in the IO monad
newKey :: IO (Key a)

-- However, we add a new top-level declaration to allocate new top-level key:
key_rng :: Key StdGen

-- Each thread has an implicit, thread-specific dictionary:
getDict :: IO Dict
setDict :: Dict  -> IO a -> IO a

-- standard idiom of global
globalVar :: IORef Int
{-# NOINLINE myGlobalVar #-}
globalVar = unsafePerformIO (newIORef 17)

-- This idiom is popularly known as "the unsafePerformIO hack". Everyone agrees that it is unsatisfactory. Aside from its ugliness, it uses unsafePerformIO in an unsafe manner: the meaning of the program is likely to change if any occurrence of globalVar is replaced with the rhs of the declaration. (In contrast, other "approved" uses of unsafePerformIO are perfectly safe.) The NOINLINE pragma used above happens to prevent unsafe program transformations in GHC and Hugs, but there's no theoretical justification for the correctness of the idiom. The idea of mutable global variables is theoretically sound, so shouldn't there be a theoretically sound way to create them?


-- Don't do that

-- A library with global variables can be mechanically translated into a library with explicit state parameterization and a function Lib.init :: IO Lib.State.

-- The parameter can be hidden with a state monad, with implicit parameters, or with implicit configurations. Furthermore this translation makes the library more versatile than before, since multiple state universes can be created and used safely.

type MyIO a = (?ref :: IORef Int) => IO a

{-
    Objections:

    There are cases in which this parameterization costs convenience and gains nothing. For example, the standard library function (randomIO :: Random a => IO a) cannot be implemented in Haskell without the unsafePerformIO hack, yet there's nothing semantically objectionable about it. A random number function written in pure Haskell 98 would therefore have to work some other way, for example by passing the random number generator or a list of numbers it generates around between functions, which many would find inconvenient. Worse, we may weaken type checking by the translation, while gaining nothing: for example, the interface below cannot provide static guarantees like fromAtom a == fromAtom b => a == b if toAtom is parameterized by an "atom space". (Note this is not a == b <=> fromAtom a == fromAtom b as there is no guarantee toAtom is called on every String, and the guarantee above can be made for the parameterized version if we use the type system to ensure the two Atoms compared are from the same "atom space"):

    data Atom = ...
    toAtom :: String -> Atom
    fromAtom :: Atom -> String
    instance Eq Atom
    
    It is argued that having multiple state spaces is sometimes semantically incorrect in a more absolute sense, but this is disputed. It's not clear that there are any problems that are naturally modeled by a unique mutable variable per process; usually the natural level of abstraction is either higher (in the operating system's domain) or lower (multiple state spaces per process). However, there are many efficiency hacks such as memoization tables or association tables which do have a natural per-process scope.
    
    Several real-life examples of pure haskell code which need fast global variables to either be implemented efficiently or statically guarantee their invariants (for the given API) are given in http://www.haskell.org//pipermail/haskell/2004-November/014929.html
-}

-- Top-level <-
-- The implementation guarantees that the rhs of the arrow is performed at most once. We don't lose beta reduction because we don't define a beta reduction rule for top-level <- in the first place.
x <- newIORef 'c'

{-
    High-level objections to this proposal:

    Though there are cases where using truly global (per-process) mutable variables appears to make sense, such cases are rare. Is it really a good idea to introduce syntax to make their creation easy and unobtrusive? Something uglier and more verbose might actually be preferable.
    The naive formulation of this proposal immediately runs into difficulties.

    We can easily break the type system by creating a top-level IORef with a polymorphic type (a problem shared with the unsafePerformIO hack); furthermore it's not clear how (possibly circular) dependencies between top-level actions should be handled. We can solve these problems by treating the entire program as a single mdo statement. A more serious problem is that there is nothing to prevent arbitrary observable IO actions from appearing to the right of the arrow. If we perform all actions before executing main, then import becomes a side-effectful operation, rather than simply a way of bringing names into scope;
    furthermore we must specify the order in which actions from different modules are executed, which would appear to be difficult in general. If we execute actions on demand (as the unsafePerformIO hack does) then we are building an unsafe syntactic construct into the language.

    A definition of the semantics of such bindings via a translation to haskell building on the work of recursive monads was written by JohnMeacham and is available:

    http://www.haskell.org//pipermail/haskell/2004-October/014618.html

    An efficient implementation strategy is also proposed. It should be noted that this implementation can be used as the basis for any of the subproposals below with just minor changes to the translation to restrict the allowed initializers types as appropriate.

    Several subproposals have appeared to address this problem.

    3.1 Proposal 2(a): Don't worry about it
    We could trust programmers to do the right thing, restricting themselves to sensible IO actions like newIORef.

    Objections:

    This breaks Haskell's tradition of bondage and discipline. If we're going to trust the programmer, we ought at least to require her to write the word "unsafe" somewhere. As a client of a library I want a reasonable guarantee that it's not performing arbitrary IO actions behind my back on startup.
    3.2 Proposal 2(b): Use forall s. ST s
    Instead of expecting top-level actions to have the type IO a for some a, we could require the type forall s. ST s a. The compiler checks that the type is polymorphic enough, and then passes it through stToIO (which is a safe function) before executing it. On its own this proposal doesn't work. If we write x <- newSTRef 'c' then x will end up with the type STRef RealWorld Char instead of the desired IORef Char. This can be fixed by MergingStAndIo (which appears to be sound, and is probably a good idea even if this proposal is not adopted).
    3.3 Proposal 2(c): Use a new monad
    We can create a new monad called ACIO which contains just the operations which are safe to perform in any order and at any time at the top level, and then require that top-level actions belong to ACIO.

    AC stands for Affine Central.

    An IO action u is affine if its effect is not indirectly observable, hence need not be performed if the result is unneeded. That is, if u >> v === v for all actions v.
    It is central if its effect commutes with every other IO action. That is,

    if do { x <- u; y <- v; w } === do { y <- v; x <- u; w } for all actions v and w.
    Objections:

    Because Haskell lacks subtyping, we need a new monad with its own primitive operations and a lifting function to IO. The primitive operations duplicate operations already defined (with different names and slightly different types) in IO and (perhaps) ST, and the lifting function is a variation of id. Users must remember to write x <- newACIORef 'c' at the top level but x <- newIORef 'c' elsewhere. Do we gain anything from this vis-a-vis 2(b), which reduces the number of monads instead of increasing it, and has just a single newRef function?
-}

-- Type-based dictionaries and execution contexts

{-
    We can use the  Data.Dynamic module, which is provided by many Haskell implementations, to implement a dictionary of values indexed by type. Global variables can be implemented by making having a single such dictionary available to the whole program.
    
    A refinement of this idea is to implement what are known as execution contexts. Each execution context is associated with a type-based dictionary. We can run an action within a new, empty, execution context. This appeals to those who would like the Haskell run-time system to be able to run sub-programs which do not clobber each other's internal state. In the long run such an approach could also prove useful in a highly parallel world where we want different processors to have different mutable state, so that updating it doesn't become a bottle-neck in the system.

    An prototype implementation already exists. An example of how we might use it to implement a function getNextNatural generating unique non-negative integers follows.
-}

data UniqueNaturalSource 
   = UniqueNaturalSource (IORef Integer) deriving (Typeable)
 
mkUniqueNaturalSource :: IO UniqueNaturalSource
mkUniqueNaturalSource =
   do
      ioRef <- newIORef 1
      return (UniqueNaturalSource ioRef)
 
getNextNatural :: IO Integer
getNextNatural =
   do
      (UniqueNaturalSource ioRef) <- lookupWithRegister mkUniqueNaturalSource
      atomicModifyIORef ioRef (\ i -> (i+1,i))

{-
    The dictionary lookup function is  lookupWithRegister, which accesses a variable from the current execution context, if necessary creating it with the action it takes as an argument (mkUniqueNaturalSource in this case).
    Advantages:

    Type-based Dictionaries and Execution Contexts do not require the Haskell language to be extended, However they would need a safe global variable for their hash table, provided perhaps by the FFI, new compiler primitives, or proposal 2 above.
    Arbitrary IO actions can be used as initialisers.
    The order in which things happen is well-defined.
    Can be built upon the primitives provided by option 2.
    
    Disadvantages:

    As in the above example, it is advisable that all global variables should be wrapped in a special-purpose data or newtype, so that no-one else can clobber them.
    Where you really do want a single global variable, as with the Atom proposal above, this approach is going to be somewhat less efficient than that of Proposal (2), because of the need to look things up in the type-based dictionary. It should be possible to make the look-up reasonably fast by storing the values in a large hash table with hash function the unique integer key for a  TypeRep already created internally by Glasgow Haskell, but even so this should be at least a little less efficient than an implementation where global state can be accessed statically could be. It would also be hard for the compiler to see what is going on, so it wouldn't be able to, for example, replace multiple lookups of the same variable by a single lookup.
    Certain C libraries really need a global variable which has only one copy throughout the program, however many execution contexts there are. To implement this using the above approach, you would need a special separate type-based dictionary for such variables. Likewise you might in some cases want a type-based dictionary for a single processor or thread. It might however be argued that the ability to manage multiple type dictionaries instead of constraining everyone to use the same "execution context" is actually an advantage rather than disadvantage of this proposal.
    It is impossible to implement this function efficiently in Haskell 98 with only the standard libraries, though you could implement an inefficient version which only worked for Read/Show types by storing the values in a temporary file.
    The prototype implementation uses unsafePerformIO to set up the original execution context, in the same way that, for example, implementations of the Haskell library typically use unsafePerformIO to initialise the standard random number generator.
-}

-- Shared on-demand IO actions (oneShots)
{-
    The problems arise because of interactions of substitution/sharing and IO. Currently, we can share descriptions of IO actions, resulting in multiple executions of such descriptions after substitution, or we can share the results of executing IO actions, by executing them before we continue.

    What we cannot do is to share the IO actions themselves, ie bind an IO action to a name, and ensure that the IO action is executed at most once, on demand, with shared results.

    If we add support for such shared on-demand IO actions to the language (top-level or not), we can use that support to share IO-based initialisation code (ensuring it is executed only once), and we can share IO-based creation of MVars (ensuring that all shared references refer to the same MVar).
-}
-- Assuming library support for oneShot IO actions

mkOnceIO :: IO a -> IO (IO a)
mkOnceIO io = do
  mv <- newEmptyMVar
  demand <- newEmptyMVar
  forkIO (takeMVar demand >> io >>= putMVar mv)
  return (tryPutMVar demand () >> readMVar mv)

-- usage, sugared
myGlobalVar =< newIORef 42
 
main = do 
  gv <- myGlobalVar
  doSomeThing gv

-- usage, desugared 
{-# OPTIONS_GHC -fno-cse #-} 
{-# NOINLINE myGlobalVar #-}
myGlobalVar :: IO (IORef Int)
myGlobalVar = unsafePerformIO (mkOnceIO (newIORef 42))
 
main = do 
  gv <- myGlobalVar
  doSomeThing gv

{-
    Notes:

    apart from the global option, this is an entirely local transformation.
    only the mkOnceIO is performed unsafely, not the shared IO action itself.
    the type is IO (IORef Int), so myGlobalVar remains an IO action, even if shared, and can only be invoked within the IO monad.
    there is nothing global about this proposal; it is only that using such shared on-demand IO actions at top-level, possibly exported from the defining module, allows us to address the issues discussed on this page
-}

-- Rather than the '=<' syntax used above, we can eliminate the need for syntactic sugar altogether by using "deriving" clauses. First, we define two classes:

class OnceInit a where
   onceInit :: IO a
 
class OnceInit a => OnceIO a where
   runOnceIO :: IO a

-- Then, to define a source of unique integers, the programmer would say:

newtype UniqueRef = UniqueRef (IORef Integer)
                       deriving OnceIO
 
instance OnceInit UniqueRef where
   onceInit = liftM UniqueRef (newIORef 0)
 
uniqueInt :: IO Integer
uniqueInt = do
   UniqueRef uniqueRef <- runOnceIO
   n <- readIORef uniqueRef
   writeIORef uniqueRef (n+1)
   return n

{-
    The OnceInit class lets the programmer specify how a type is initialized; above, it just allocates a new IORef, but we could also read a configuration file or parse command-line arguments, for example. In contrast, instances of the OnceIO class are not written by the programmer; instead, they are generated automatically by a "deriving OnceIO" clause. Each type for which OnceIO is derived will have a special top-level action associated with it, which is accessed through the runOnceIO function. Its semantics are:

    The first time that "runOnceIO" is called, it runs the corresponding "onceInit" action and caches and returns the result.
    Every subsequent time that "runOnceIO" is called, it returns the cached result.
    This behavior is safe precisely because runOnceIO is an IO action. Even though one can't guarantee when in the program an initialization will occur, when the initialization does happen it will be sequenced among other IO actions.
-}

-- Internally, the OnceIO instance might be expanded (with associated NOINLINE/NOCSE) as

instance Once UniqueRef where
   runOnceIO = modifyMVar onceUniqueRef $ \mx -> case mx of
       Just x -> return (Just x, x)
       Nothing -> do {x <- onceInit; return (Just x, x)}
 
onceUniqueRef = unsafePerformIO $ newMVar Nothing

-- Three meanings of "global"

    -- 1. A global mutable cell might be one which has a single identity which persists for the life of the program (or the life of a thread, or ...)
    -- 2. A global mutable cell might be one which is visible (can be referred to) from anywhere in the program.
    -- 3. A global mutable cell is just and extension of the world implicitly used in the IO Monad.

{-
    The "global" in the title of this page refers to the first sense. Globalness in the second sense is orthogonal to the question of mutable state. (Or is it?)

    Global mutable cells (in the first sense) have also been called "top-level things with identity". I chose the title of this page because it seemed more likely to correctly suggest to newcomers the problem being addressed, even though it may be strictly speaking less accurate.

    6.1 Comments
    (AHEY) To be honest I think the use of the term "global variable" is quite inappropriate for the proposals under discussion, at least for proposal 2. We're talking about allowing mutable data structures at the program top level. Whether or not the scope of a particular top level mutable data structure is "global" (exported from a module), locally visible throughout just one module, or buried inside a closure (doesn't even scope over an entire module) is an entirely separate issue.
    Consider the following example:

    -- No objections to this I assume
    oneShot :: IO a -> ACIO (IO a)
    oneShot io = mdo mv <- newMVar $ do a <- io
                                        let loop = do putMVar mv loop
                                                    return a
                                        loop
                    return $ do act <- takeMVar mv
                                act
    
    -- Using the hypothetical new top level <- bindings (proposal 2c)
    initialiseItOnceOnly :: IO Result
    initialiesItOnceOnly <- oneShot initialialiseIt
    The MVar created by oneShot certainly is not what I would call "global". It doesn't even scope over an entire module. The action "initialiseItOnceOnly" might well be global (exported from the defining module). But there's no problem here. It's type is (IO Result), so we already know it has potentially unknown state mutating effects. What difference does it make if part of the state it mutates is top level Haskell state rather than "real world state" (whatever that might be)?

    There is no difference between these two kinds of state. As has already been observed, allowing top level mutable state in Haskell is really just a way of allowing programmers to augment the initial "real world state" (seen by main) with user defined state.

    Another possible use might be to create raw top level mutable data structures aren't buried in closures but instead scope over an entire module. A simple example being ..

    myIORef :: IORef Int
    myIORef <- newIORef 0
    Certainly it would be inappropriate to export myIORef (this really would be creating a global variable in the classic sense). But I see no harm in this so long as it's scope is confined to a single module and what is actually exported is a robust API consisting of a limited and carefully chosen set of actions which are able to read or write myIORef.

    A third possibility is that top level mutable data structures are exported, but as abstract data types. So although they are "global", the only thing outside users can do with them is supply them as arguments to a a robust API consisting of a limited and carefully chosen set of actions which are able to inspect or modify those data structures. For example..

    -- Code that would be typical in (for example) a device driver for an
    -- embedded application with known and fixed hardware resources running NoOS. 
    
    data DeviceHandle        -- Exported as abstract
    data DeviceSessionHandle -- Exported as abstract
    
    -- This is *not* exported because there is no possibility of
    -- user code creating its own devices and it is *dangerous* to
    -- allow this. There must be one and only one unique packet of
    -- state associated with each device.
    createDeviceHandle :: BaseAddress -> ACIO DeviceHandle
    
    -- So we create the two available device handles at top level.
    -- These *are* exported..
    device1,device2 :: DeviceHandle
    device1 <- createDeviceHandle device1BaseAddress
    device2 <- createDeviceHandle device2BaseAddress
    
    -- .. as is the common device API
    someActionWithADevice           :: DeviceHandle -> IO ()
    someOtherActionWithADevice      :: DeviceHandle -> ... -> IO Bool
    openAStatefulSessionWithADevice :: DeviceHandle -> IO DeviceSessionHandle
    someActionInAStatefulSession    :: DeviceSessionHandle -> IO ()
    Of course there other possible usage scenarios. But all these examples seem quite harmless to me. In fact thay are better than harmless because they provide a sound and perfectly safe solution to common (and currently insoluble) problems encountered when dealing with low level IO libraries.

    - Adrian Hey

    7 Initialisation and the ACIO monad
    One problem with using the ACIO monad at the top level is that you cannot do any real IO. I think something like this is neccessary if we are to keep the feel of a declarative language. But it leads to a problem if any real world resource associated with a top level mutable data structure requires IO for initialisation (forking of a separate driver thread for example).

    Typically we would want to ensure that:

    Initialisation has occurred before any attempt to use the device.
    Initialisation occurs only once so existing device state doesn't get screwed up by accidental re-initialisation (or forking multiple driver threads all trying to control the same device).
    Users don't need to worry about any of this.
    In the device driver code example, if the createDeviceHandle function has to do some initialisation it must be in the IO monad, so we couldn't create device handles at the top level. But I think we could do something almost as good using the "oneShot" function.

    -- Almost the same as before but uses IO instead of ACIO (not exported).
    createDeviceHandle :: BaseAddress -> IO DeviceHandle
    
    -- Instead of top level device handles we have top level actions (exported).
    getDevice1, getDevice2 :: IO DeviceHandle
    getDevice1 <- oneShot $ createDeviceHandle device1BaseAddress
    getDevice2 <- oneShot $ createDeviceHandle device2BaseAddress
    - Adrian Hey

    8 Possible solution to the thread forking initialisation problem
    The last example solution was kind of OK, but it did prevent having device handles themselves as top level identifiers which can be exported from a module. Now, the real problem is not with the forking of a thread as such, it's what happens when that thread starts running that's the problem. We could get around this by ensuring the forked thread was blocked until some real IO operation set it running.

    Suppose the device driver thread provided it's services via a channel and responded if necessary via some kind of call-back mechanism (using continuations passed in the requests).

    newtype DeviceHandle = DeviceHandle (Chan Request) -- Exported as abstract
    
    -- General Driver thread, parameterised by device hardware address and request action
    driverThread :: BaseAddress -> Chan Request -> IO ()
    
    -- Start the driver for a device at a particular base address
    -- (This is not exported)
    startDriver :: BaseAddress -> IO DeviceHandle
    startDriver base = do chan <- newChan         -- newChan could be in ACIO monad
                        forkIO (driverThread base chan) 
                        return (DeviceHandle chan)
    As before, we have no way stopping accidental forking of two or more driver threads for the same device. But suppose instead we have..

    -- This time the driver thread takes it's first request as an additional argument
    driverThread :: BaseAddress -> Request-> Chan Request  -> IO ()
    
    -- Start the driver for a device, initially blocked on Request Channel
    -- (This is not exported)
    startDriver :: BaseAddress -> IO DeviceHandle
    startDriver base = do chan <- newChan         -- newChan could be in ACIO monad
                        forkIO $ do first <- readChan chan
                                    driverThread base first chan  
                        return (DeviceHandle chan)
    Since the forked thread cannot do anything until something is written to the channel, the act of starting the driver itself has no observable IO effects. So (apart from typing issues) there's no reason why we shouldn't have..

    -- Start the driver for a device, initially blocked on the Request Channel
    -- (This is not exported)
    startDriver :: BaseAddress -> ACIO DeviceHandle
    startDriver base = do chan <- forkWithChanACIO (driverThread base) -- forkWithChanACIO is a new primitive
                        return (DeviceHandle chan)
    
    -- These are exported..
    device1,device2 :: DeviceHandle
    device1 <- startDriver device1BaseAddress
    device2 <- startDriver device2BaseAddress
    The new primitive cannot be implemented without some kind of "dodgyIOtoACIO" cast.
    I use the prefix dodgy rather than unsafe because it is nowhere near as unsafe as

    unsafePerformIO (which should really be called unsoundPerformIO IMO). In this case, the implementation of forkWithChanACIO could be..
    forkWithChanACIO :: (a -> Chan a -> IO ()) -> ACIO (Chan a) 
    forkWithChanACIO thread = do chan <- newChan  -- newChan is in ACIO monad this time
                                dodgyIOtoACIO $ forkIO $ do a <- readChan chan
                                                            thread a chan
                                return chan
    You could have other similar primitives, like forkWithMVarACIO.
-}