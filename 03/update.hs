k :: Int -> Int
k 0 = 0
k _ = 1

update :: Eq a => (a -> b) -> a -> b -> a -> b
update f a b a'
    | a == a'   = b
    | otherwise = f a'

k2 :: Int -> Int
k2 = foldl (\acc x -> update acc x 0) k [2, 5]

symm :: (Int -> Int) -> Int -> Int
symm f x
    | x < 0     = f (-x)
    | otherwise = f x

main = print $ map (symm (^3)) [-3, -2 .. 3]
