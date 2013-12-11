pyTriples :: [(Int, Int, Int)]
pyTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], c^2 == a^2 + b^2]

main = print $ take 10 pyTriples
