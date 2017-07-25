-- Hint and Mueval
-- GHC itself can actually interpret arbitrary Haskell source on the fly by hooking into the GHC's bytecode interpreter ( the same used for GHCi ). The hint package allows us to parse, typecheck, and evaluate arbitrary strings into arbitrary Haskell programs and evaluate them.

import Language.Haskell.Interpreter

foo :: Interpreter String
foo = eval "(\\x -> x) 1"

example :: IO (Either InterpreterError String)
example = runInterpreter foo