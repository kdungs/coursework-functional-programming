import Data.Char(toUpper)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:[]) = True
isSorted (x:y:xs)
    | x > y     = False
    | otherwise = isSorted (y:xs)

camelCase :: String -> String
camelCase [] = []
camelCase (x:[]) = [x]
camelCase (x:y:xs)
    | x == '_'  = toUpper y : camelCase xs
    | otherwise = x : camelCase (y:xs)

main = print $ camelCase "some_random_method"
