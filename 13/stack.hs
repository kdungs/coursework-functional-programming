newtype Trans s a = T { run :: s -> (a,s) }

instance Monad (Trans s) where
    return a = T $ \s -> (a,s)
    T trans >>= f = T $ \s -> let (a,s') = trans s
                              in run (f a) s'

data Stack a = Stack [a] deriving Show

isEmpty :: Trans (Stack a) Bool
isEmpty = T $ \(Stack xs) -> (null xs, Stack xs)

pop :: Trans (Stack a) a
pop = T $ \(Stack (x:xs)) -> (x, Stack xs)

push :: a -> Trans (Stack a) ()
push x = T $ \(Stack xs) -> ((), Stack (x:xs))

example :: Stack Int 
example = (snd . run m . Stack) [1,2,3,4] where
    m :: Trans (Stack Int) ()
    m = do
        pop
        x <- pop
        push 7
        push 9
        pop
        push x

main = do
    print example