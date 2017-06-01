-- Ternary operator
-- ref: https://wiki.haskell.org/Ternary_operator

import qualified Prelude
 
data Cond a = a : a
 
infixl 0 ?
infixl 1 :
 
(?) :: Prelude.Bool -> Cond a -> a
Prelude.True  ? (x : _) = x
Prelude.False ? (_ : y) = y
 
test = 1 Prelude.< 2 ? "yeah" : "no!"

data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?
 
(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
 
test = 1 < 2 ? "Yes" :? "No"