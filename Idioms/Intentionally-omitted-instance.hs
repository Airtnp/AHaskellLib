-- Intentionally omitted instance
-- ref: https://wiki.haskell.org/Intentionally_omitted_instance

-- but this is certainly not a good idea. Is there a way to tell the user that the instance was left unimplemented by intention? Actually, this is possible with current Haskell extensions by implementing an instance with a depending instance that a user cannot fulfill:

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Char (Char) where
 
class Unimplementable a where
 
instance Unimplementable Char => Num Char where
   (+) = undefined
   (*) = undefined
   abs = undefined
   signum = undefined
   fromInteger = undefined

-- The important point is to not export the Unimplementable class. It is ugly that you have to enable UndecidableInstances for this trick, and you still have to suppress warnings about unimplemented methods. Thus it might be better have language support for prohibition of certain instances like in GHC ticket 9334: "Instance chains".