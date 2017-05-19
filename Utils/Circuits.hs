-- Circuits
-- ref: http://www.cse.unt.edu/~tarau/teaching/cf1/Syn.pdf
-- Quipper
-- ref: http://www.mathstat.dal.ca/~selinger/quipper/doc/Quipper.html
-- Cλash
-- ref: http://www.clash-lang.org/
-- isomorphism circuits
-- ref: https://rahne.si/programming/clash/fpga/2016/12/23/iso-circuits.html

module Circuits where
import Data.List
import Data.Bits
import Data.Array

-- (.&.) :: Bits a => a -> a -> a
-- bits and

type N = Integer

nand_ :: N -> N -> N -> N
nor_ :: N -> N -> N -> N
impl_ :: N -> N -> N -> N
less_ :: N -> N -> N -> N
and_ :: N -> N -> N -> N

nand_ mask x y = mask .&. (complement (x .&. y))
nor_ mask x y = mask .&. (complement (x .|. y))
impl_ mask x y = (mask .&. (complement x)) .|. y
less_ _ x y = x .&. (complement y)
and_ _ x y = x .&. y

opcode m 0 = nand_ m
opcode m 1 = nor_ m
opcode m 2 = impl_ m
opcode m 3 = less_ m
opcode _ 4 = xor
opcode m 5 = and_ m
opcode _ n = error ("unexpected opcode:"++(show n))
opname 0 = "nand"
opname 1 = "nor"
opname 2 = "impl"
opname 3 = "less"
opname 4 = "xor"
opname 5 = "and"
opname n = error ("no such opcode:"++(show n))

symops = [0,1]
asymops = [2,3]
impl_and = [2,5]

allOnes nvars = 2^2^nvars - 1

var_n n k = var_mn (allOnes n) n k
var_mn mask n k = mask ‘div‘ (2^(2^(n-k-1))+1)

encode_var m n k 
    | k == n = m
    | k == n+1 = 0
    | otherwise = var_mn m n k

init_inputs n =
    0:m:(map (encode_var m n) [0..n-1]) where
        m = allOnes n

decode_var nvars v 
    | v == (allOnes nvars) = nvars
    | v == 0 = nvars + 1
    | otherwise = head
        [k|k←[0..nvars-1],(encode_var m nvars k)==v] where 
            m = allOnes nvars

bindings 0 us = [[]]
bindings n us = [zs|ys←bindings (n-1) us,zs←map (:ys) us]

generateVarMap occs vs = map (listArray (0,occs-1)) (bindings occs vs)

data T a = V a | F a (T a) (T a) 
    deriving (Show, Eq)

generateT lib n = unfoldT lib n 0

unfoldT _ 1 k = [V k]
unfoldT lib n k = [F op l r |
    i  <- [1..n-1],
    l  <- unfoldT lib i k,
    r  <- unfoldT lib (n-i) (k+i),
    op <- lib]

foldT _ g (V i) = g i
foldT f g (F i l r) = f i (foldT f g l) (foldT f g r)

fsize t = foldT f g t where
    g _ = 0
    f _ l r = 1+l+r

decodeV nvars is i = V (decode_var nvars (is!i))
decodeF i x y = F i x y
decodeResult nvars (leafDAG,varMap,_) =
    foldT decodeF (decodeV nvars varMap) leafDAG

showT nvars t = foldT f g t where
    g i =
        if i < nvars
        then "x" ++ (show i)
        else show (nvars+1-i)
    f i l r =(opname i) ++ "(" ++ l ++ "," ++ r ++ ")"

buildAndEvalLeafDAG lib nvars maxleaves =
    [(leafDAG,varMap,eval varMap leafDAG) |
        k <- [1..maxleaves],
        varMap <- generateVarMap k vs,
        leafDAG <- generateT lib k
    ] where
        mask = allOnes nvars
        vs = init_inputs nvars
        eval varMap leafDAG = foldT (opcode mask) (varMap!) leafDAG

findFirstGood lib nvars maxleaves ttn =
    head [r|r <-
        buildAndEvalLeafDAG lib nvars maxleaves,
        testspec ttn r
    ] where testspec spec (_,_,v) = spec == v

synthesize_from lib nvars maxleaves ttn =
    decodeResult nvars candidate where
        candidate = findFirstGood lib nvars maxleaves ttn

synthesize_with lib nvars ttn =
    synthesize_from lib nvars (allOnes nvars) ttn

syn lib nvars ttn = (show ttn) ++ ":" ++ (showT nvars (synthesize_with lib nvars ttn))
synall lib nvars = map (syn lib nvars) [0..(allOnes nvars)]

-- examples
t0 = findFirstGood symops 3 8 71
t1 = syn asymops 3 71
t2 = mapM_ print (synall asymops 2)
t3 = syn symops 3 83
t4 = syn asymops 3 83
t5 = syn [0..4] 3 83 -- ite with all ops
-- x xor y xor z -- cpu intensive
t6 = syn asymops 3 105
