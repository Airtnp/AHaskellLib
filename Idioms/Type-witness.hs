-- Type witness
-- ref: https://wiki.haskell.org/Type_witness

-- A type witness is a value that represents one of a range of possible types. When implemented as generalised algebraic datatypes (GADTs), type witness can be used to perform dynamic casts.

-- Types a and b may or may not be the same type. We wish to perform the cast a -> Maybe b, or more generally, p a -> Maybe (p b) for any p, depending on whether a and b are the same type. This can be done with GADTs if we have "witnesses" for the two types:

dynamicCast :: Witness a -> Witness b -> p a -> Maybe (p b)

-- A simple witness type might be defined over the Int, Bool and Char types like so:

data Witness a where
  IntWitness :: Witness Int
  BoolWitness :: Witness Bool
  CharWitness :: Witness Char
 
dynamicCast IntWitness IntWitness pa = Just pa
dynamicCast BoolWitness BoolWitness pa = Just pa
dynamicCast CharWitness CharWitness pa = Just pa
dynamicCast _ _ _ = Nothing

-- If we wish to add type constructors such as the list constructor, that can be done by composing type constructors:

data Witness a where
  IntWitness :: Witness Int
  BoolWitness :: Witness Bool
  CharWitness :: Witness Char
  ListWitness :: Witness a -> Witness [a] 
 
data Compose p q a = MkCompose (p (q a))
 
dynamicCast IntWitness IntWitness pa = Just pa
dynamicCast BoolWitness BoolWitness pa = Just pa
dynamicCast CharWitness CharWitness pa = Just pa
dynamicCast (ListWitness wa) (ListWitness wb) pla = do
  MkCompose plb <- dynamicCast wa wb (MkCompose pla)
  return plb
dynamicCast _ _ _ = Nothing

-- By using more complex composition types in addition to Compose, it is possible for witnesses to allow type constructors of more complex kinds.

