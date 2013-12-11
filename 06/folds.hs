filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p = fst .
    foldr (\x (l, s) -> if (s || p x) then (x:l, s) else (l, True)) ([], False)

main = print $ filterLast (< 4) [1,2,3,4]
