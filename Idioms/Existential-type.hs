-- Existential type
-- ref: https://wiki.haskell.org/Existential_type
-- trac: https://prime.haskell.org/wiki/ExistentialQuantification

-- 1. Intro

    -- Normally when creating a new type using type, newtype, data, etc., every type variable that appears on the right-hand side must also appear on the left-hand side. Existential types are a way of turning this off.

    -- monomorphism restriction

-- no data Worker b x y = Worker {buffer :: b, input :: x, output :: y}
data Worker x y = forall b. Buffer b => Worker {buffer :: b, input :: x, output :: y}

    -- In general, when you use a 'hidden' type in this way, you will usually want that type to belong to a specific class, or you will want to pass some functions along that can work on that type. Otherwise you'll have some value belonging to a random unknown type, and you won't be able to do anything to it!

    -- Note: You can use existential types to convert a more specific type into a less specific one. (See the examples below.) There is no way to perform the reverse conversion!

-- 2. Examples

data Obj = forall a. (Show a) => Obj a
 
xs :: [Obj]
xs = [Obj 1, Obj "foo", Obj 'c']
 
doShow :: [Obj] -> String
doShow [] = ""
doShow ((Obj x):xs) = show x ++ doShow xs

-- raytracer

-- In a raytracer, a requirement is to be able to render several different objects (like a ball, mesh or whatever).
class Renderable a where
    boundingSphere :: a -> Sphere
    hit :: a -> [Fragment] -- returns the "fragments" of all hits with ray
    {- ... etc ... -}

hits :: Renderable a => [a] -> [Fragment]
hits xs = sortByDistance $ concatMap hit xs

-- However, this does not work as written since the elements of the list can be of SEVERAL different types (like a sphere and a polygon and a mesh etc. etc.) but lists need to have elements of the same type.

-- solutions
{-# OPTIONS -fglasgow-exts #-}
 
data AnyRenderable = forall a. Renderable a => AnyRenderable a

instance Renderable AnyRenderable where
    boundingSphere (AnyRenderable a) = boundingSphere a
    hit (AnyRenderable a) = hit a

-- Dynamic dispatch of OOP

-- Existential types in conjunction with type classes can be used to emulate the dynamic dispatch mechanism of object oriented programming languages. To illustrate this concept I show how a classic example from object oriented programming can be encoded in Haskell.

class Shape_ a where
   perimeter :: a -> Double
   area      :: a -> Double
 
data Shape = forall a. Shape_ a => Shape a

type Radius = Double
type Side   = Double

data Circle    = Circle    Radius
data Rectangle = Rectangle Side Side
data Square    = Square    Side


instance Shape_ Circle where
    perimeter (Circle r) = 2 * pi * r
    area      (Circle r) = pi * r * r

instance Shape_ Rectangle where
    perimeter (Rectangle x y) = 2*(x + y)
    area      (Rectangle x y) = x * y

instance Shape_ Square where
    perimeter (Square s) = 4*s
    area      (Square s) = s*s

instance Shape_ Shape where
    perimeter (Shape shape) = perimeter shape
    area      (Shape shape) = area      shape


--
-- Smart constructor
--

circle :: Radius -> Shape
circle r = Shape (Circle r)

rectangle :: Side -> Side -> Shape
rectangle x y = Shape (Rectangle x y)

square :: Side -> Shape
square s = Shape (Square s)

shapes :: [Shape]
shapes = [circle 2.4, rectangle 3.1 4.4, square 2.1]

-- GADT parse

-- Exceptions

-- Control.Exception (see documentation) provides extensible exceptions by making the core exception type, SomeException, an existential:

class (Show e, Typeable e) => Exception e where
    toException :: e -> SomeException
    fromException :: SomeException -> Maybe e
data SomeException = forall a. Exception a => SomeException a

-- You can define your own exceptions by making them an instance of the Exception class. 

{-
    Then there are two basic ways of dealing with exceptions:
    
    If you have a SomeException value, use fromException. This returns Just e if the exception is the type you want. If it's something else, you get Nothing. You could check multiple types using a guard. This is what you'll have to use if you're dealing with SomeExceptions in pure code.
    
    If you're in IO and have an expression that might throw an exception, catch lets you catch it. (There's also a version generalised to other instances of MonadIO in the lifted-base package). Its second argument takes a handler, which is a function accepting an exception of the type you want. If the first argument throws an exception, catch uses the Typeable library's typesafe cast to try to convert it to the type you want, then (if it succeeded) passes it to the handler. You can apply catch many times to the same expression with handlers for different exception types.
    
    Even if fromException doesn't turn up an exception type you know, and catch doesn't catch an exception type you know, you can still show the unknown exception, maybe after catching SomeException.
-}

-- 3. Alternate methods

-- 3.1 Concrete data types

    -- 3.1.1 Universal instance of a Class
    -- Here one way to simulate existentials (Hawiki note: (Borrowed from somewhere...))

 type Point = (Float,Float)
 
 class Shape a  where
	draw :: a -> IO ()
	translate :: a-> Point -> a

-- Then we can pack shapes up into a concrete data type like this:

data SHAPE = SHAPE (IO ()) (Point -> SHAPE)

-- with a function like this

packShape :: Shape a => a -> SHAPE
packShape s = SHAPE (draw s) (\(x,y) -> packShape (translate s (x,y)))

instance Shape SHAPE where
    draw (SHAPE d t) = d
    translate (SHAPE d t) = t

    -- 3.1.2 Using constructors and combinators

data Shape = Shape {
    draw :: IO()
    translate :: (Int, Int) -> Shape
 }

 -- Then you can create a library of shape constructors and combinators that each have defined "draw" and "translate" in their "where" clauses.

 circle :: (Int, Int) -> Int -> Shape
 circle (x,y) r =
    Shape draw1 translate1
    where
       draw1 = ...
       translate1 (x1,y1) = circle (x+x1, y+y1) r
 
 shapeGroup :: [Shape] -> Shape
 shapeGroup shapes = Shape draw1 translate1
    where
       draw1 = mapM_ draw shapes
       translate1 v = shapeGroup $ map (translate v) shapes

-- 3.2 Cases that really require existentials

data Expr a = Val a | forall b . Apply (Expr (b -> a)) (Expr b)
data Action = forall b . Act (IORef b) (b -> IO ())

-- (Maybe this last one could be done as a type Act (IORef b) (IORef b -> IO ()) then we could hide the IORef as above, that is go ahead and apply the second argument to the first)

-- 3.3 Existentials in terms of "forall"

-- It is also possible to express existentials as type expressions directly (without a data declaration) with RankNTypes.

data Obj = forall a. (Show a) => Obj a
-- the type Obj is equivalent to: 
-- forall r. (forall a. Show a => a -> r) -> r
-- (the leading forall r. is optional unless the expression is part of another expression). The conversions are:
fromObj ::  Obj 
        -> forall r. (forall a. Show a => a -> r) -> r
fromObj (Obj x) k = k x
 
toObj :: (forall r. (forall a. Show a => a -> r) -> r) 
      ->  Obj
toObj f = f Obj
