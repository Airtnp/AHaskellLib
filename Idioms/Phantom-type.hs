-- Phantom type
-- ref: https://wiki.haskell.org/Phantom_type

-- A phantom type is a parametrised type whose parameters do not all appear on the right-hand side of its definition, e.g. from Control.Applicative:

newtype Const a b = Const { getConst :: a }

-- Here Const is a phantom type, because the b parameter doesn't appear after the = sign.

-- Phantom types are useful in a variety of contexts: in Data.Fixed they are used with type classes to encode the precision being used, with smart constructors or GADTs they can encode information about how and where a value can be used, it may help to avoid kind signatures or with more exotic extensions they can be used for encoding bounds checks in the type system. Since the values of type parameters in a phantom type may be unused, they are often used in combination with empty types.

-- Phantom types are nearly always either newtype or data. It is possible to create "phantom type synonyms", but they are usually useless: since synonyms are expanded at compile time, the phantom type variable will be discarded.

data FormData a = FormData String

changeType :: FormData a -> FormData b
changeType (FormData str) = FormData str

data Validated
data Unvalidated
 
-- since we don't export the constructor itself,
-- users with a String can only create Unvalidated values
formData :: String -> FormData Unvalidated
formData str = FormData str
 
-- Nothing if the data doesn't validate
validate :: FormData Unvalidated -> Maybe (FormData Validated)
validate (FormData str) = ...
 
-- can only be fed the result of a call to validate!
useData :: FormData Validated -> IO ()
useData (FormData str) = ...

-- The beauty of this is that we can define functions that work on all kinds of FormData, but still can't turn unvalidated data into validated data:

-- the library exports this
liftStringFn :: (String -> String) -> FormData a -> FormData a
liftStringFn fn (FormData str) = FormData (fn str)
 
-- the validation state is the *same* in the return type and the argument
dataToUpper :: FormData a -> FormData a
dataToUpper = liftStringFn (map toUpper)

-- With type classes, we can even choose different behaviours conditional on information that is nonexistent at runtime:

class Sanitise a where
  sanitise :: FormData a -> FormData Validated
 
-- do nothing to data that is already validated
instance Sanitise Validated where
  sanitise = id
 
-- sanitise untrusted data
instance Sanitise Unvalidated where
  sanitise (FormData str) = FormData (filter isAlpha str)

-- This technique is perfect for e.g. escaping user input to a web application. We can ensure with zero overhead that the data is escaped once and only once everywhere that it needs to be, or else we get a compile-time error.

-- We create a Parameterized type in which the parameter does not appear on the rhs (shameless cutting and pasting from Daan Leijen and Erik Meijer)

data Expr a = Expr PrimExpr
 
constant :: Show a => a -> Expr a
(.+.)  :: Expr Int -> Expr Int -> Expr Int
(.==.) :: Eq a=> Expr a-> Expr a-> Expr Bool
(.&&.) :: Expr Bool -> Expr Bool-> Expr Bool
 
data PrimExpr
  = BinExpr   BinOp PrimExpr PrimExpr
  | UnExpr    UnOp PrimExpr
  | ConstExpr String
 
data BinOp
  = OpEq | OpAnd | OpPlus | ...

-- Why not type synonyms

-- Remember that type synonyms are expanded behind the scenes before typechecking. Suppose that in the above example you replace the declaration of Expr with type Expr a = PrimExpr. Then Expr Int and Expr String are both expanded to PrimExpr before being compared, and those types would be compatible, defeating the point of using a phantom type.

