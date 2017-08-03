-- Liquid Haskell

-- LiquidHaskell is an extension to GHC's typesystem that adds the capacity for refinement types using the annotation syntax. The type signatures of functions can be checked by the external for richer type semantics than default GHC provides, including non-exhaustive patterns and complex arithmetic properties that require external SMT solvers to verify. For instance LiquidHaskell can statically verify that a function that operates over a Maybe a is always given a Just or that an arithmetic functions always yields an Int that is even positive number.

