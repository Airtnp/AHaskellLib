-- Timing
-- ref: https://wiki.haskell.org/Timing_computations
-- ref: https://wiki.haskell.org/Timing_computation_in_cycles
-- ref: https://wiki.haskell.org/Timing_out_computations

-- Timing an IO computation -- very basic approach. For a full featured, statistically sound benchmarking system, see the criterion package.

import Text.Printf
import Control.Exception
import System.CPUTime
 
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

{- 
main = do
    putStrLn "Starting..."
    time $ product [1..10000] `seq` return ()
    putStrLn "Done."
-}

-- Timing a pure computation:

import Text.Printf
import Control.Exception
import System.CPUTime
import Control.Parallel.Strategies
import Control.Monad
import System.Environment
 
lim :: Int
lim = 10^6
 
time :: (Num t, NFData t) => t -> IO ()
time y = do
    start <- getCPUTime
    replicateM_ lim $ do
        x <- evaluate $ 1 + y
        rnf x `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.9f sec\n" (diff :: Double)
    printf "Individual time: %0.9f sec\n" (diff / fromIntegral lim :: Double)
    return ()

{-
main = do
    [n] <- getArgs
    let y = read n
    putStrLn "Starting..."
    time (y :: Int)
    putStrLn "Done."
-}

-- rdtsc

import System.CPUTime.Rdtsc
 
import qualified Data.Map as M
import Text.Printf
import System.Environment

{-
list =
    ["Baughn" ,"falconair" ,"Lemmih" ,"Philippa" ,"ToRA" ,"bd_" ,"felipe" ,"levitation[A"
    ,"Plareplane" ,"TSC" ,"bdash" ,"flux__" ,"lisppaste2" ,"Poeir" ,"tuukkah" ,"beelsebob"
    ,"fnordus" ,"lispy" ,"prb" ,"twanvl" ,"benc__" ,"fridim_" ,"liyang" ,"profmakx"
    ,"TwigEther" ,"benja_" ,"gaal" ,"LoganCapaldo" ,"Prozen" ,"uebayasi" ,"Betovsky"
    ,"gdsx" ,"lokadin" ,"psnl" ,"Ugarte" ,"bitshifter" ,"GeoBesh" ,"Lor" ,"pstickne" ,"ulfdoz"
    ,"bobwhoops" ,"george--" ,"loud-" ,"psykotic" ,"vegai" ,"bohanlon" ,"giksos" ,"lucca"
    ,"ptolomy" ,"vegaiW" ,"bos" ,"glguy" ,"Lunar^" ,"Pupeno" ,"Vq^" ,"bos31337" ,"gmh33"
    ,"Lunchy" ,"PupenoR" ,"waern" ,"Botje" ,"grumpy_old_one" ,"magagr" ,"pyronicide"
    ,"Wallbraker" ,"boulez" ,"GueNz" ,"mahogny" ,"quazaway" ,"wchogg" ,"cajole" ,"guillaumh"
    ,"makinen" ,"quetzal" ,"wilx`" ,"Cale" ,"gvdm_other" ,"MarcWeber" ,"qwr" ,"woggle"
    ,"calvins_" ,"Hirvinen" ,"maskd" ,"rafl" ,"wolverian" ,"cameron" ,"ibid" ,"mathrick"
    ,"ramkrsna" ,"xerox" ,"carp_" ,"Igloo" ,"mathrick_" ,"ramza3" ,"Xgc" ,"clanehin" ,"ikaros"
    ,"mattam" ,"rashakil_" ,"xian" ,"ClaudiusMaximus" ,"integral" ,"matthew-_" ,"ray"
    ,"xinming" ,"clog" ,"Jaak" ,"mauke" ,"rc-1" ,"xpika" ,"cmeme" ,"jbalint" ,"mbishop"
    ,"rds" ,"yaarg" ,"Codex_" ,"jcreigh" ,"metaperl" ,"reppie" ,"yosemite" ,"cods" ,"jdev"
    ,"Mitar" ,"resiak" ,"Z4rd0Z" ,"cognominal" ,"jgrimes" ,"mlh" ,"retybok" ,"zamez" ,"cpfr"
    ,"JKnecht" ,"moconnor" ,"rey_" ,"zeuxis" ,"ctkrohn" ,"jlouis" ,"monochrom" ,"saccade"
    ,"ziggurat" ,"daniel_larsson" ,"jmg_" ,"moonlite" ,"Saizan" ,"|shad0w|" ,"dany2k" ,"jmob"
    ,"mornfall" ,"SamB" ,"Daveman" ,"joene_" ,"mr_ank" ,"SamB_XP" ,"dblog" ,"johs_" ,"ms_"
    ,"scw" ,"dcoutts" ,"jrockway" ,"Muad_Dib" ,"shapr" ]
 
main = do
    [v] <- getArgs
 
    -- force evaluation (don't want to time evaluation of lazy structures)
    length list `seq` return ()
    let m = M.fromList (zip list [1..])
    M.size m `seq` return ()
 
    -- do the lookup
    t <- rdtsc
    k <- M.lookup v m
    u <- rdtsc
 
    print k
    printf "%d cycles\n" (fromIntegral (u - t) :: Int)
-}

{-

    A problem that frequently arises is that of constructing an IO action which terminates after a given period of time, regardless of whether it has finished the computation it was intended to perform. There are a variety of ways in which this can be accomplished.

    One implementation for various efficient timeout combinators can be found in HAppS.Util.TimeOut (GPL licenced).

    One way is to consider the more general problem of executing a number of competing actions in parallel, and returning the result of the first to be finished its task, an interesting problem in and of itself.

    What we do is to create an initially empty MVar, spawn threads for each of the competing computations, and have them all compete to execute their action and then put the result into the MVar. In the main thread we try to take from the MVar, which blocks until one of the threads completes its task. We then kill all of the threads, and return the winning result.

-}

import Control.Concurrent
 
compete :: [IO a] -> IO a
compete actions = do
    mvar <- newEmptyMVar
    tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
    result <- takeMVar mvar
    mapM_ killThread tids
    return result

-- In order to implement the timeout, we just have two processes compete: the one to attempt, with its result wrapped in the Just constructor, and one which waits the specified time (in microseconds) and then returns Nothing.

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = compete [fmap Just action, threadDelay usec >> return Nothing]

-- A related problem is that of iterating a pure function for as many steps as possible until a given time limit passes, and returning the last computed result afterward. One can achieve this again using threads as follows:

import Control.Concurrent
import Control.Exception
 
timeoutIterate msec f x = do
    mvar <- newMVar x
    let loop = do
           x <- takeMVar mvar
           evaluate (f x) >>= putMVar mvar
           loop
    thread <- forkIO loop
    threadDelay msec
    u <- takeMVar mvar
    killThread thread
    return u

-- Without threads, using getClockTime to check if enough time has passed, it looks like this:

import Control.Exception
import System.Time
 
getClockTimeMS = do
    (TOD s p) <- getClockTime
    return $ fromIntegral (s * 1000 + p `div` 10^6)
 
timeoutIterate' msec f x = do
    t <- getClockTimeMS
    y <- evaluate (f x)
    t' <- getClockTimeMS
    timeoutIterate (msec - (t' - t)) f y

-- Note that in both cases, the use of evaluate is important to ensure that all of the evaluation actually occurs in the given timeframe and not lazily afterward.

-- microseconds
getClockTimeMS = do
    (TOD s p) <- getClockTime
    return $ fromIntegral (s * 1000000 + p `div` 10^6)
 
timeoutIterate msec f x = do
  t <- getClockTimeMS
  timeoutIterate' (t + msec) f x
 
timeoutIterate' fin f x = do
  t <- getClockTimeMS
  if t > fin then
      return x
    else 
      do y <- evaluate (f x)
         t' <- getClockTimeMS
         timeoutIterate' fin f y