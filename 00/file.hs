f :: Int -> Int -> Int -> Int
f x y z = x + y + z * z

addFive :: Int -> Int
addFive x = x + 5

one :: Int
one = 1

k :: Int
k = addFive $ addFive one

addTen :: Int -> Int
addTen x = addFive $ addFive x

addTenComposition :: Int -> Int
addTenComposition x = addFive . addFive $ x
