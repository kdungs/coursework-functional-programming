import Blatt09

getLeftChild :: TreeZipper a -> Maybe a
getLeftChild (_, Leaf) = Nothing
getLeftChild (_, (Bin _ Leaf _)) = Nothing
getLeftChild (_, (Bin _ (Bin x _ _) _)) = Just x

getRightChild :: TreeZipper a -> Maybe a
getRightChild (_, Leaf) = Nothing
getRightChild (_, (Bin _ _ Leaf)) = Nothing
getRightChild (_, (Bin _ _ (Bin x _ _))) = Just x

getUpper :: TreeZipper a -> Maybe a
getUpper (Top, _) = Nothing
getUpper (L _ _ (Bin x _ _), _) = Just x
getUpper (R _ (Bin x _ _) _, _) = Just x

neighbours :: TreeNode a -> [Maybe a]
neighbours n = map ($(treeToZipper n)) [getLeftChild, getUpper, getRightChild]


main = do
    print $ neighbours (tree, [0, 1])