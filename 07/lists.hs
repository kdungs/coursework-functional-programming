data List a = Nil | Cons a (List a) deriving Show

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons a b) = Cons (f a) (mapList f b)

foldrList :: (a -> b -> b) -> b -> List a -> b
foldrList _ x0 Nil = x0
foldrList f x0 (Cons a b) = f a (foldrList f x0 b)

main = print $ foldrList (+) 0 (Cons 3 (Cons 7 Nil))
