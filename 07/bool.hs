data BExp = T
          | F
          | Var Int
          | Neg BExp
          | Disj BExp BExp
          | Conj BExp BExp
          deriving Show


eval :: (Int -> Bool) -> BExp -> Bool
eval _ T = True
eval _ F = False
eval f (Var i) = f i
eval f (Neg e) = not (eval f e)
eval f (Disj a b) = (eval f a) || (eval f b)
eval f (Conj a b) = (eval f a) && (eval f b)


evalListEnv :: [Bool] -> BExp -> Bool
evalListEnv vars e = eval (\i -> vars !! i) e


environments :: Int -> [[Bool]]
environments 0 = [[]]
environments n = [True:e | e <- ens] ++ [False:e | e <- ens]
    where ens = environments (n - 1)

equiv :: Int -> BExp -> BExp -> Bool
equiv n a b = (evalAll a) == (evalAll b)
    where evalAll ex = map (flip evalListEnv ex) en
          en = environments n

main = do
    print $ evalListEnv [True, False] (Conj (Var 0) (Var 1))
    print $ evalListEnv [True, False] (Disj (Var 0) (Var 1))
    print $ environments 3
    print $ equiv 2 (Conj (Var 0) (Var 1)) (Disj (Var 0) (Var 1))
