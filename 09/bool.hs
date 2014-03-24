data BExp a = T
            | F
            | Var a
            | Neg (BExp a)
            | Disj (BExp a) (BExp a)
            | Conj (BExp a) (BExp a)
            deriving Show

--instance (Eq a) => Eq (BExp a) where
--    T == T = True
--    T == F = False
--    F == T = False
--    F == F = True
--    Var a == Var b = a == b
--    Neg a == b = a /= b
--    a == Neg b = a /= b
--    Disj a b = c == 


varMap :: (a -> b) -> BExp a -> BExp b
varMap _ T = T
varMap _ F = F
varMap f (Var x) = Var (f x)
varMap f (Neg x) = Neg (varMap f x)
varMap f (Disj x y) = Disj (varMap f x) (varMap f y)
varMap f (Conj x y) = Conj (varMap f x) (varMap f y)


main = do
    print $ test
    print $ varMap (\x -> 4 - x) test
    where test = Conj (Conj (Var 0) (Var 1)) (Disj (Var 0) (Var 1))