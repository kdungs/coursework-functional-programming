import Control.Monad

xsComp :: [(Int, Int)]
xsComp = [(x, y) | x <- [1, 2], y <- [6, 7], x + y /= 8]

xsComp' :: [(Int, Int)]
xsComp' = do
    x <- [1, 2]
    y <- [6, 7]
    guard (x + y /= 8)
    return (x, y)

xsComp'' :: [(Int, Int)]
xsComp'' = [1,2] >>= \x -> [6, 7] >>= \y -> guard (x + y /= 8) >> return (x, y)

xsComp''' :: [(Int, Int)]
xsComp''' = concat (map 
    (\x -> concat (map
        (\y -> guard (x + y /= 8) >> return (x, y))
        [6, 7]))
    [1, 2])


main = do
    print $ xsComp
    print $ xsComp'
    print $ xsComp''
    print $ xsComp'''
