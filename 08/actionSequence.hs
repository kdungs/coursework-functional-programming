type Point = (Float, Float)
type Path = [Point]

data Action = Turn Float | Move Float deriving Show

angle :: Float -> Point -> Point -> Float
angle a (x1, y1) (x2, y2) = (f (y2 - y1) (x2 - x1) * 180 / pi) - a where
    f 0 0 = atan2 0 1
    f x y = atan2 x y

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

optimiseActions :: [Action] -> [Action]
optimiseActions [] = []
optimiseActions (Move 0:xs) = optimiseActions xs
optimiseActions (Turn 0:xs) = optimiseActions xs
optimiseActions (x:[]) = [x]
optimiseActions (Turn a:Turn b:xs) = Turn (a + b):optimiseActions xs
optimiseActions (Move a:Move b:xs) = Move (a + b):optimiseActions xs
optimiseActions (x:xs) = x:optimiseActions xs

makeActions :: Path -> [Action]
makeActions = optimiseActions . reverse . fst . foldl f ([], ((0, 0), 0)) where
    f (acc, (cp, ca)) p = (Move d:Turn a:acc, (p, a)) where
        a = angle ca cp p
        d = distance cp p


main = print $ makeActions [(1, 0), (0, 1), (0, 0)]
