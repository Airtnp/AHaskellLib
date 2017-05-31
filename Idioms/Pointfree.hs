-- Pointfree
-- ref: https://wiki.haskell.org/Pointfree
-- JN.Oliveira: http://www4.di.uminho.pt/~jno/html/jnopub.html

{-

Thomas Yaeger has written a Lambdabot plugin to automatically convert a large subset of Haskell expressions to pointfree form. This tool has made it easier to use the more abstract pointfree encodings (as it saves some mental gymnastics on the part of the programmer). You can experiment with this in the Haskell IRC channel. A stand-alone command-line version is available at HackageDB (package pointfree).

The @pl (point-less) plugin is rather infamous for using the (->) a monad to obtain concise code. It also makes use of Arrows. It also sometimes produces (amusing) code blow ups with the (.) operator.
Recently, @unpl has been written, which (attempts) to unscramble @pl-ified code. It also has a stand-alone command-line version (package pointful).



 > pl \x y -> x y
 id
 
 > unpl id
 (\ a -> a)
 
 > pl \x y -> x + 1
 const . (1 +)
 
 > unpl const . (1 +)
 (\ e _ -> 1 + e)
 
 > pl \v1 v2 -> sum (zipWith (*) v1 v2)
 (sum .) . zipWith (*)
 
 > unpl (sum .) . zipWith (*)
 (\ d g -> sum (zipWith (*) d g))
 
 > pl \x y z -> f (g x y z)
 ((f .) .) . g
 
 > unpl ((f .) .) . g
 (\ e j m -> f (g e j m))
 
 > pl \x y z -> f (g x y) z
 (f .) . g
 
 > unpl (f .) . g
 (\ d i -> f (g d i))
 
 > pl \x y z -> f z (g x y)
 (flip f .) . g
 
 > unpl (flip f .) . g
 (\ i l c -> f c (g i l))
 
 > pl \(a,b) -> (b,a)
 uncurry (flip (,))
 
 > pl f a b = b a
 f = flip id
 
 > pl \ x -> x * x
 join (*)
 
 > pl \a b -> a:b:[]
 (. return) . (:)
 
 > pl \x -> x+x+x
 (+) =<< join (+)
 
 > pl \a b -> Nothing
 const (const Nothing)
 
 > pl \(a,b) -> (f a, g b)
 f *** g
 
 > pl \f g h x -> f x `h` g x
 flip . (ap .) . flip (.)
 
 > pl \x y -> x . f . y
 (. (f .)) . (.)
 
 > pl \f xs -> xs >>= return . f
 fmap
 
 > pl \h f g x -> f x `h` g x
 liftM2
 
 > pl \f a b c d -> f b c d a
 flip . ((flip . (flip .)) .)
 
 > pl \a (b,c) -> a c b
 (`ap` snd) . (. fst) . flip
 
 > pl \x y -> compare (f x) (f y)
 ((. f) . compare .)

 > pl \(a,b) -> a:b:[]
 uncurry ((. return) . (:))
 
 > pl \a b c -> a*b+2+c
 ((+) .) . flip flip 2 . ((+) .) . (*)
 
 > pl \f (a,b) -> (f a, f b)
 (`ap` snd) . (. fst) . (flip =<< (((.) . (,)) .))
 
 > pl \f g (a,b) -> (f a, g b)
 flip flip snd . (ap .) . flip flip fst . ((.) .) . flip . (((.) . (,)) .)
 
 > unpl flip flip snd . (ap .) . flip flip fst . ((.) .) . flip . (((.) . (,)) .)
 (\ aa f ->
   (\ p w -> ((,)) (aa (fst p)) (f w)) >>=
      \ ao -> snd >>= \ an -> return (ao an))


-}

-- The owl
owl :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
owl = ((.)$(.)) 
-- owl a b c d = a b (c d)

-- Dot
dot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = ((.).(.))
-- (=<<) == join `dot` fmap
-- dot a b c d = a (b c d)

-- Swing
swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing = flip . (. flip id)
swing f = flip (f . runCont . return)
swing f c a = f ($ a) c

swing map :: forall a b. [a -> b] -> a -> [b]
swing any :: forall a. [a -> Bool] -> a -> Bool
swing foldr :: forall a b. b -> a -> [a -> b -> b] -> b
swing zipWith :: forall a b c. [a -> b -> c] -> a -> [b] -> [c]
swing find :: forall a. [a -> Bool] -> a -> Maybe (a -> Bool)
-- applies each of the predicates to the given value, returning the first predicate which succeeds, if any
swing partition :: forall a. [a -> Bool] -> a -> ([a -> Bool], [a -> Bool])

-- Squash
f >>= a . b . c =<< g
-- (readFile y >>=) . ((a . b) .) . c =<< readFile x

{-

    Point-free style can (clearly) lead to Obfuscation when used unwisely. As higher-order functions are chained together, it can become harder to mentally infer the types of expressions. The mental cues to an expression's type (explicit function arguments, and the number of arguments) go missing.

    Point-free style often times leads to code which is difficult to modify. A function written in a pointfree style may have to be radically changed to make minor changes in functionality. This is because the function becomes more complicated than a composition of lambdas and other functions, and compositions must be changed to application for a pointful function.

    Perhaps these are why pointfree style is sometimes (often?) referred to as pointless style.

-}