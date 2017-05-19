-- Compose
-- ref: https://wiki.haskell.org/Compose

-- compose :: [a -> a] -> (a -> a)
-- chaining function

-- sane solution
compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v


-- using State
composeState :: [a -> a] -> a -> a
composeState = execState . mapM modify

-- consider it in the less eta-reduced form:
-- composeState fs v = execState (mapM modify fs) v

-- mapM iterates over the list of functions, applying modify to each one. If we were to expand a list after it had been mapped over in this way,
{-
    fs = mapM modify [(*2), (+1), \n -> (n - 5) * 4]
    -- fs is entirely equivalent to the following do-block:
    fs' = do modify (*2)
            modify (+1)
            modify (\n -> (n - 5) * 4)
-}

-- using Reader
composeReader :: [a -> a] -> a -> a
composeReader fs v = runReader (compose' fs) v
   where compose' []     = ask
         compose' (f:fs) = local f (compose' fs)
-- compose' = foldr local ask
 
-- alternative: no runReader or ask required
composeReader' :: [a -> a] -> a -> a
composeReader' = foldr local id

{-
    fs = compose' [(*2), (+1), \n -> (n - 5) * 4]
    -- again, this is entirely equivalent to the following:
    fs' = local (*2) $
            local (+1) $
                local (\n -> (n - 5) * 4) ask
-}


-- using Writer
composeWriter :: [a -> a] -> a -> a
composeWriter fs v = (execWriter $ compose' fs) v 
    where compose' []     = return id
          compose' (f:fs) = censor (. f) (compose' fs)
-- compose' = foldr (censor . flip (.)) (return id)

-- ensor, to quote All About Monad, "...takes a function and a Writer and produces a new Writer whose output is the same but whose log entry has been modified by the function.".


-- using Cont

-- GOTO-style checkpoint
getCC :: MonadCont m => m (m a)
getCC = callCC (\c -> let x = c x in return x)
getCC' :: MonadCont m => a -> m (a, a -> m b)
getCC' x0 = callCC (\c -> let f x = c (x, f) in return (x0, f)

-- This takes m-sized chunks off of u (which starts off being x) until u is in range.
x `modulo` m = (`runContT` return) $ do (u, jump) <- getCC' x
                                        lift $ print u
                                        case u of
                                        _ | u < 0     -> jump (u + m)
                                          | u >= m    -> jump (u - m)
                                          | otherwise -> return u

composeCont :: [a -> a] -> a -> a
composeCont fs = runCont compose' id where 
    compose' = do ((gs,f), jump) <- getCC' (fs,id)
                    case gs of
                        []      -> return f
                        (g:gs') -> jump (gs', g . f)

