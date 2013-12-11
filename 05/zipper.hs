type ListIndex a = ([a], Int)
type ListZipper a = ([a], [a])

listToZipper :: ListIndex a -> ListZipper a
listToZipper = loop []
    where loop :: [a] -> ([a], Int) -> ([a], [a])
          loop c (s, 0)   = (c, s)
          loop c (a:s, n) = loop (a:c) (s, n - 1)

zipperToList :: ListZipper a -> ListIndex a
zipperToList (c, s) = loop c (s, 0)
    where loop :: [a] -> ([a], Int) -> ([a], Int)
          loop (a:c) (s, n) = loop c (a:s, n + 1)
          loop _ sn         = sn

back, forth :: ListZipper a -> ListZipper a
back (a:c, s) = (c, a:s)
forth (c, a:s) = (a:c, s)

toZipper :: [a] -> ListZipper a
toZipper l = listToZipper (l, 0)

fromZipper :: ListZipper a -> [a]
fromZipper (s, n) = reverse s ++ n

update :: (a -> a) -> ListZipper a -> ListZipper a
update f (c, a:s) = (c, (f a):s)

main = print $ fromZipper $ update (\_ -> 'u') $ forth $ forth $ toZipper "hallo"
