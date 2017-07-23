-- Type Inhabitation proof
-- ref: https://www.zhihu.com/question/41925054

{-# LANGUAGE RankNTypes #-}

type FALSE = forall b. b

proof :: (forall a. (a -> Bool) -> a) -> FALSE
proof f = f (\x -> True)

{-

注：
    1. FALSE定义成forall b. b，因为forall b. b可以推出一切。你也可以用其它的FALSE定义，比如forall b. (b -> b) -> b，或者更复杂的。
    2. (a -> Bool) -> a是forall a. (a -> Bool) -> a的缩写。
    3. 对于任何一个类型t，如果你能写出t -> FALSE的（非死循环，不抛出异常，不调用unsafeXX）的Haskell函数，那么等效于你构造出了t -> False的证明项，也就是说你证明了t -> False，即t为假，即类型t中无值。

-}