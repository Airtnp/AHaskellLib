import System.IO
import Data.IORef
import System.IO.Unsafe
import Control.Monad
import Data.Function
import Control.Exception

data Unit

type Void = IO ()

type Counter = (Unit -> IO Int, (Unit -> Void, Int -> Void))
type CounterRep = (IO (IORef Int), Unit)

-- r = (newIORef (1 :: Int), ())
newCounter :: CounterRep -> Counter 
newCounter = \r' -> let r = r'
                        get :: Unit -> IO Int
                        get = \_ -> (fst r) >>= readIORef
                        inc :: Unit -> IO ()
                        inc = \_ -> (fst r) >>= flip modifyIORef (+1)
                        set :: Int -> IO ()
                        set = \i -> (fst r) >>= flip writeIORef i
                    in (get, (inc, set))

superCounter :: CounterRep -> Counter
superCounter = \r' -> let super = newCounter r'
                          get = fst super
                          inc :: Unit -> IO ()
                          inc = \_ -> (fst r') >>= flip modifyIORef (+2)
                          set = snd $ snd super
                      in (get, (inc, set))

-- or just CounterRep -> Counter -> Counter
selfCounter :: CounterRep -> Counter
selfCounter = \r' -> let r = r'
                         get :: Unit -> IO Int
                         get = \_ -> (fst r) >>= readIORef
                         set :: Int -> IO ()
                         set = \i -> (fst r) >>= flip writeIORef i
                         recur :: Counter -> Counter                         
                         recur = \self -> (get,  ((\x -> (fst self $ x) >>= \x -> evaluate (x + 1) >>= (snd $ snd self)) , set))
                     in fix recur