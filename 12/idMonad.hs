data Id a = Id { get :: a}

instance Monad Id where
    return = Id
    Id x >>= f = f x

mapId :: (a -> b) -> [a] -> Id [b]
mapId f xs= Id (map f xs)


mapp :: (a -> b) -> [a] -> [b]
mapp f = get . mapId f


main = do
    print $ mapp (*2) [1..10]