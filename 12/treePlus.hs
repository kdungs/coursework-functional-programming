import Control.Monad

data Bintree a = Empty | Leaf a | Branch a ( Bintree a) ( Bintree a)


satisfiesFromRootToLeaf :: (a -> Bool) -> Bintree a -> [a]
satisfiesFromRootToLeaf _ Empty = []
satisfiesFromRootToLeaf p (Leaf l) = if p l then [l] else []
satisfiesFromRootToLeaf p (Branch x l r) = if p x
    then (satisfiesFromRootToLeaf p l) ++ (satisfiesFromRootToLeaf p r)
    else []


satisfiesFromRootToLeafMplus :: MonadPlus m => (a -> Bool) -> Bintree a -> m a
satisfiesFromRootToLeafMplus _ Empty = mzero
satisfiesFromRootToLeafMplus p (Leaf l) = if p l then return l else mzero
satisfiesFromRootToLeafMplus p (Branch x l r) = if p x
    then (satisfiesFromRootToLeafMplus p l) `mplus` (satisfiesFromRootToLeafMplus p r)
    else mzero


main = do
    print $ satisfiesFromRootToLeaf (/=7) (Branch 4 (Leaf 1) (Leaf 9))
    print $ satisfiesFromRootToLeaf (/=4) (Branch 4 (Leaf 1) (Leaf 9))
    print $ satisfiesFromRootToLeaf (/=1) (Branch 4 (Leaf 1) (Leaf 9))
    print $ (satisfiesFromRootToLeafMplus (/=7) (Branch 4 (Leaf 1) (Leaf 9)) :: Maybe Int)
    print $ (satisfiesFromRootToLeafMplus (/=4) (Branch 4 (Leaf 1) (Leaf 9)) :: Maybe Int)
    print $ (satisfiesFromRootToLeafMplus (/=1) (Branch 4 (Leaf 1) (Leaf 9)) :: Maybe Int)
    print $ (satisfiesFromRootToLeafMplus (/=7) (Branch 4 (Leaf 1) (Leaf 9)) :: [Int])
    print $ (satisfiesFromRootToLeafMplus (/=4) (Branch 4 (Leaf 1) (Leaf 9)) :: [Int])
    print $ (satisfiesFromRootToLeafMplus (/=1) (Branch 4 (Leaf 1) (Leaf 9)) :: [Int])
