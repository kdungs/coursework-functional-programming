import Control.Monad


lookupMonadPlus :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupMonadPlus _ [] = mzero
lookupMonadPlus k ((a, b):xs)
    | a == k    = return b `mplus` rest
    | otherwise = rest
    where rest = lookupMonadPlus k xs


main = do
    print $ (lookupMonadPlus 3 [(3, 1), (3, 2), (5, 9)] :: Maybe Int)
    print $ (lookupMonadPlus 0 [(3, 1), (3, 2), (5, 9)] :: Maybe Int)
    print $ (lookupMonadPlus 3 [(3, 1), (3, 2), (5, 9)] :: [Int])
    print $ (lookupMonadPlus 0 [(3, 1), (3, 2), (5, 9)] :: [Int])