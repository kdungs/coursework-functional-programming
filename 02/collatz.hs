collatz :: Int -> Int
collatz n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1

countStepsToOne :: Int -> Int
countStepsToOne n = length (takeWhile (/=1) (iterate collatz n))

main = print $ countStepsToOne 10
