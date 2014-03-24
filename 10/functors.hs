data Bintree a  = Leaf
                | Branch a (Bintree a) (Bintree a)
                deriving Show

instance Functor Bintree where
    fmap _ Leaf = Leaf
    fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)


doubleValues :: Functor f => f Int -> f Int
doubleValues = fmap (*2)


sumUpAssociatedToValues :: [Int] -> [(Int, Int)] -> Maybe Int
sumUpAssociatedToValues keys pairs = fmap sum $ sequence (
                                        map (flip lookup pairs) keys)

main = do
    print $ tree
    print $ fmap (>10) tree
    print $ doubleValues tree
    print $ doubleValues [1..10]
    print $ doubleValues (Just 10)
    print $ doubleValues (Nothing)
    print $ "sumUpAssociatedToValues"
    print $ sumUpAssociatedToValues [0] l
    print $ sumUpAssociatedToValues [] l
    print $ sumUpAssociatedToValues [3] l
    print $ sumUpAssociatedToValues [3,1] l
    where tree = (Branch 10 Leaf (Branch 4 Leaf Leaf))
          l = [(1, 2), (3, 8)]