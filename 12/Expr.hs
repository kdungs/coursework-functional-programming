{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

-- 31.1.2014

import Control.Monad
import Painter (just,get,readFileAndDo,Tree(V,F),root,mkInt,Rules,reduce,
        drawTerm)

-- download http://fldit-www.cs.uni-dortmund.de/~peter/Haskellprogs/Painter.tgz

import Data.Data
import Data.Generics

-- Hilfsfunktionen

update :: Eq a => (a -> b) -> a -> b -> a -> b
update f a b x = if x == a then b else f x

updList :: [a] -> Int -> a -> [a]
updList s i a = take i s++a:drop (i+1) s

fold2 f a (x:xs) (y:ys) = fold2 f (f a x y) xs ys
fold2 _ a _ _           = a
   
listsToFun = fold2 update . const
           
mkSet :: Eq a => [a] -> [a]
mkSet = foldl (flip insert) [] 

insert :: Eq a => a -> [a] -> [a] 
insert x s@(y:s') = if x == y then s else y:insert x s'
insert x _        = [x]

remove :: Eq a => a -> [a] -> [a] 
remove x (y:s) = if x == y then remove x s else y:remove x s
remove _ _     = []
    
type Store = String -> Int

-- sat, some, many

sat :: MonadPlus m => m a -> (a -> Bool) -> m a
sat p f = do a <- p; guard $ f a; return a

some, many :: MonadPlus m => m a -> m [a]
some p = do a <- p; as <- many p; return $ a:as
many p = some p `mplus` return []

msumOrReturn :: MonadPlus m => a -> [m a] -> m a
msumOrReturn a ms = msum ms `mplus` return a

-- Transitionsmonaden

newtype Trans state a = T {run :: state -> (a,state)}

instance Monad (Trans state) where
         T trans >>= f = T $ \st -> let (a,st') = trans st 
                    in run (f a) st'
         return a = T $ \st -> (a,st)

newtype TransM state m a = TM {runM :: state -> m (a,state)}

instance MonadPlus m => Monad (TransM state m) where
         TM trans >>= f = TM $ \st -> do (a,st) <- trans st
                     runM (f a) st
         return a = TM $ \st -> return (a,st)
     fail _ = mzero
           
instance MonadPlus m => MonadPlus (TransM state m) where
         mzero = TM $ const mzero
         TM m `mplus` TM m' = TM $ \st -> m st `mplus` m' st


-- ARITHMETISCHE AUSDR†CKE

data Expr = Con Int | Var String | Sum [Expr] | Prod [Expr] | Expr :- Expr | 
        Int :* Expr | Expr :^ Int | Expr :/ Expr
        deriving (Read,Show,Eq,Typeable,Data)
        
zero = Con 0
one  = Con 1
        
data BExpr = True_ | False_ | BVar String | Or [BExpr] | And [BExpr] |
             Not BExpr | (:<) Expr Expr | (:=) Expr Expr | (:<=) Expr Expr
         deriving (Read,Show,Eq,Typeable,Data)

-- Expr-Interpreter 1

evalE :: Expr -> Store -> Int
evalE (Con i) _    = i
evalE (Var x) st   = st x
evalE (Sum es) st  = sum $ map (flip evalE st) es
evalE (Prod es) st = product $ map (flip evalE st) es
evalE (e :- e') st = evalE e st - evalE e' st
evalE (i :* e) st  = i * evalE e st
evalE (e :^ i) st  = evalE e st ^ i

-- Expr-Compiler 1

compileE :: Expr -> [StackCom]
compileE (Con i)   = [Push i]
compileE (Var x)   = [Load x]
compileE (Sum es)  = concatMap compileE es++[Add $ length es]
compileE (Prod es) = concatMap compileE es++[Mul $ length es]
compileE (e :- e') = compileE e++compileE e'++[Sub]
compileE (i :* e)  = Push i:compileE e++[Mul 2]
compileE (e :^ i)  = compileE e++[Push i,Pow]

-- Zielkommandos
     
data StackCom = Push Int | Load String | Sub | Add Int | Mul Int | Pow
            deriving Show
        
-- Interpreter der Zielkommandos

type State = ([Int],Store)

executeCom :: StackCom -> State -> State
executeCom (Push a) (stack,store) = (a:stack,store)
executeCom (Load x) (stack,store) = (store x:stack,store) 
executeCom Sub st                 = executeOp (foldl1 (-)) 2 st
executeCom (Add n) st             = executeOp sum n st
executeCom (Mul n) st             = executeOp product n st
executeCom Pow st                 = executeOp (foldl1 (^)) 2 st
                        
executeOp :: ([Int] -> Int) -> Int -> State -> State
executeOp f n (stack,store) = (f (reverse as):bs,store)
                              where (as,bs) = splitAt n stack

execute :: [StackCom] -> State -> State
execute = flip $ foldl $ flip executeCom

-- Compiler
        
type Compiler = TransM String Maybe

parse :: Compiler a -> String -> Maybe a
parse p str = do (a,_) <- runM p str; Just a
                    
getChr :: Compiler Char
getChr = TM $ \str -> do c:str <- return str; return (c,str)

char :: Char -> Compiler Char
char chr = sat getChr (== chr)

string :: String -> Compiler String
string = mapM char

token :: Compiler a -> Compiler a
token comp = do space; a <- comp; space; return a
         where space = many $ sat getChr (`elem` " \t\n")

bool :: Compiler Bool
bool = msum [do token $ string "True"; return True,
             do token $ string "False"; return False]

nat,int :: Compiler Int
nat = do ds <- some $ sat getChr (`elem` ['0'..'9']); return $ read ds
int = nat `mplus` do char '-'; n <- nat; return $ -n

identifier :: Compiler String 
identifier = do first <- sat getChr (`elem` ['a'..'z']++['A'..'Z'])
                rest <- many $ sat getChr (`notElem` "(){};=!>+-*^ \n\t")
            return $ first:rest
        
tchar = token . char
             
-- generischer Expr-Compiler

data ExprAlg a = ExprAlg {con       :: Int -> a,
              var       :: String -> a,
              sum_,prod :: [a] -> a,
              sub       :: a -> a -> a,
              scal      :: Int -> a -> a,
              expo      :: a -> Int -> a}

exprC :: ExprAlg a -> Compiler a
exprC alg = do e <- summand; moreSummands e
     where summand = do e <- factor; moreFactors e
           factor  = msum [do x <- token identifier; power $ var alg x,
                       do i <- token int; scalar i,
                       do tchar '('; e <- exprC alg; tchar ')'; power e]
           moreSummands e = msum [do tchar '-'; e' <- summand
                     moreSummands $ sub alg e e',
                      do es <- some $ do tchar '+'; summand
                         moreSummands $ sum_ alg $ e:es,
                  return e]
           moreFactors e  = msum [do es <- some $ do tchar '*'; factor
                             moreFactors $ prod alg $ e:es,
                  return e]
           power e  = msum [do tchar '^'; i <- token int; return $ expo alg e i,
                    return e]
           scalar i = msum [do tchar '*'; e <- summand; return $ scal alg i e,
                            power $ con alg i]
      
parsAlg :: ExprAlg Expr
parsAlg = ExprAlg Con Var Sum Prod (:-) (:*) (:^)

evalAlg :: ExprAlg (Store -> Int)
evalAlg = ExprAlg {con = \i -> const i,
           var = \x st -> st x,
           sum_ = \es st -> sum $ map ($st) es,
           prod = \es st -> product $ map ($st) es,
           sub = \e e' st -> e st - e' st,
           scal = \i e st -> i * e st,
           expo = \e i st -> e st ^ i}

treeAlg :: ExprAlg (Tree String)
treeAlg = ExprAlg {con = \i -> leaf i,
           var = \x -> F x [],
           sum_ = \es -> F "+" es,
           prod = \es -> F "*" es,
           sub = \e e' -> F "-" [e,e'],
           scal = \i e -> F "*" [leaf i,e],
           expo = \e i -> F "^" [e,leaf i]} 
      where leaf i = F (show i) []
               
compAlg :: ExprAlg [StackCom] 
compAlg = ExprAlg {con = \i -> [Push i],
               var = \x -> [Load x],
           sum_ = \es -> concat es++[Add $ length es],
           prod = \es -> concat es++[Mul $ length es],
           sub = \e e' -> e++e'++[Sub],
           scal = \i e -> Push i:e++[Mul 2],
                   expo = \e i -> e++[Push i,Pow]}

-- Normalisierung

type Estate = (Int,[Expr],Expr -> Int)

updState :: Estate -> Expr -> Int -> Estate
updState (c,bases,f) e i = (c,insert e bases,update f e $ f e+i)

applyL :: ([Expr] -> Expr) -> [Expr] -> Expr
applyL _ [e] = e
applyL f es  = f es

reduceE :: Expr -> Expr
reduceE (e :- e')       = reduceE $ Sum [e,(-1):*e']
reduceE (i :* Con j)    = Con $ i*j
reduceE (0 :* e)        = zero
reduceE (1 :* e)        = reduceE e
reduceE (i :* (j :* e)) = (i*j) :* reduceE e
reduceE (i :* e)        = i :* reduceE e
reduceE (Con i :^ j)    = Con $ i^j
reduceE (e :^ 0)        = one
reduceE (e :^ 1)        = reduceE e
reduceE ((e :^ i) :^ j) = reduceE e :^ (i*j)
reduceE (e :^ i)        = reduceE e :^ i
reduceE (Sum es) = case (c,map summand bases) of (_,[]) -> Con c
                         (0,es) -> applyL Sum es
                         (_,es) -> applyL Sum $ Con c:es
       where summand e = if i == 1 then e else i :* e where i = scal e
         (c,bases,scal) = foldl trans (0,[],const 0) $ map reduceE es
         trans state@(c,bases,scal) e = case e of Con 0 -> state
                                              Con i -> (c+i,bases,scal)
                                                      i:*e -> updState state e i
                              _ -> updState state e 1
reduceE (Prod es) = case (c,map factor bases) of (_,[]) -> Con c
                             (1,es) -> applyL Prod es
                         (_,es) -> c :* applyL Prod es
       where factor e = if i == 1 then e else e :^ i where i = expo e
         (c,bases,expo) = foldl trans (1,[],const 0) $ map reduceE es
         trans state@(c,bases,expo) e = case e of Con 1 -> state
                                  Con i -> (c*i,bases,expo)
                              e:^i -> updState state e i
                              _ -> updState state e 1
reduceE e = e

-- Symbolische Differentiation mit anschlie§ender Normalisierung

diff :: String -> Expr -> Expr
diff x (Con _)   = zero
diff x (Var y)   = if x == y then one else zero
diff x (Sum es)  = reduceE $ Sum $ map (diff x) es
diff x (Prod es) = reduceE $ Sum $ map f [0..length es-1]
           where f i = reduceE $ Prod $ updList es i $ diff x $ es!!i
diff x (e :- e') = reduceE $ diff x e :- diff x e'
diff x (i :* e)  = reduceE $ i :* diff x e
diff x (e :^ i)  = reduceE $ i :* reduceE (Prod [diff x e,e:^(i-1)])

diffStep :: Rules String
diffStep (F "$" [F "diff" [V x],F "Con" [t]]) | just i
                    = Just $ F "Con" [mkInt 0]
                       where i = parse int $ root t
diffStep (F "$" [F "diff" [V x],F "Var" [V y]]) 
                    = Just $ F "Con" [if x == y then mkInt 1
                                    else mkInt 0]
diffStep (F "$" [F "diff" [V x],F "Sum" es])
                    = Just $ F "Sum" $ map (mkDiff x) es
diffStep (F "$" [F "diff" [V x],F "Prod" es]) 
                    = Just $ F "Sum" $ map f [0..length es-1]
                                  where f i = F "Prod" $ updList es i 
                                   $ mkDiff x $ es!!i
diffStep (F "$" [F "diff" [V x],F ":-" [e,e']]) 
                    = Just $ F ":-" [mkDiff x e,mkDiff x e']
diffStep (F "$" [F "diff" [V x],F ":*" [t,e]]) | just i
                    = Just $ F ":*" [t,mkDiff x e]
                       where i = parse int $ root t  
diffStep (F "$" [F "diff" [V x],F ":^" [e,t]]) | just i
                            = Just $ F ":*" [t,F "Prod" [mkDiff x e,e']]
                              where i = parse int $ root t
                            e' = F ":^" [e,mkInt $ get i-1]
                        
diffStep (F "Sum" es) | any (== zero) es = Just $ F "Sum" $ filter (/= zero) es
                           where zero = F "Con" [mkInt 0]
diffStep (F "Sum" [e])               = Just e
diffStep (F "Prod" es) | any (== one) es = Just $ F "Sum" $ filter (/= one) es
                           where one = F "Con" [mkInt 1]
diffStep (F "Prod" [e])          = Just e
diffStep (F ":*" [t,F ":*" [u,e]]) | just i && just j 
                         = Just $ F ":*" [v,e]
                           where i = parse int $ root t
                                 j = parse int $ root u
                             v = mkInt $ get i*get j
diffStep _ = Nothing

mkDiff x e = F "$" [F "diff" [V x],e]

-- Ausgabe

showE :: Expr -> String
showE = flip f "" where f :: Expr -> String -> String
            f (Con i)         = (show i++)
                    f (Var x)         = (x++)
            f (Sum es)        = fs '+' es
            f (Prod es)       = fs '*' es
            f (e :- e')       = enclose $ f e . ('-':) . f e' 
            f (n :* e)        = enclose $ (show n++) . ('*':) . f e 
            f (e :^ n)        = enclose $ f e . ('^':) . (show n++) 
            
            fs :: Char -> [Expr] -> String -> String
            fs op (e:es) = enclose $ f e . foldr trans id es
                                   where trans e h = (op:) . f e . h
            fs _ _       = id
            
            enclose :: (String -> String) -> String -> String
            enclose f = ('(':) . f . (')':)

-- Beispiele

x = Var "x"
y = Var "y"
z = Var "z"

t1 = Sum [5 :* Con 11,6 :* Con 12,Prod [x,y,z]]        -- 5*11+6*12+x*y*z
t2 = Sum [Prod [x,y,z], Con 127]               -- x*y*z+127
t3 = Sum [11 :* (x :^ 3),5 :* (x :^ 2),16 :* x,Con 33] -- 11*x^3+5*x^2+16*x+33
t4 = Sum [11 :* (x :^ 3),5 :* (x :^ 2),16 :* x,Con 33,22 :* (x :^ 2)] 
t5 = Sum [5 :* (x :^ 2),22 :* (x :^ 2)] 
t6 = Prod [x :^5,Prod [x :^5,x:^6]]

exp1 = "5*11+6*12+x*y*z"
exp2 = "11*x^3+5*x^2+16*x+33"
exp3 = "x^3*x^5"
exp4 = "5*(6*(11*(x+y+z)*14+(c+g+b)*22)+44*(gg+hh))"
exp5 = "x-x-x"
exp6 = "11*x^3*x^4+33+3*x+5*x^2+16*x+(6*x^2+55)*5*x^2"
exp7 = "x^4+5*x^3+11*x^2+222"

diff1 = reduce "diff" [diffStep]

-- compile "exp1" 0 --> t1  
-- compile "exp1" 1 --> t3 -showE-> (127+(x*y*z))
-- showE $ diff "x" t3  --> (16+(33*(x^2))+(10*x))
-- compile "exp2" 2     --> (16+(33*(x^2))+(10*x))
-- compile "exp7" 2     --> (16+(33*(x^2))+(10*x))
-- compile "exp2" 3..6  --> (x = 5 --> 1613)

compile :: String -> Int -> IO ()
compile file n = readFileAndDo file h
   where h str = case n of 0 -> act (exprC parsAlg) $ showExp id
               1 -> act (exprC parsAlg) $ showExp reduceE
               2 -> act (exprC parsAlg) $ showExp $ diff "x"
               3 -> act (exprC parsAlg >>= return . evalE) loopE
               4 -> act (exprC evalAlg) loopE
               5 -> act (exprC parsAlg >>= return . compileE) loopC
               _ -> act (exprC compAlg) loopC
           where act :: Compiler a -> (a -> IO ()) -> IO ()
         act comp continue = case runM comp str of 
                     Just (a,"") -> continue a
                         Just (a,str) 
                           -> do putStrLn $ "unparsed suffix: "++str
                                 continue a
                             _ -> putStrLn "syntax error"
                
showExp :: (Expr -> Expr) -> Expr -> IO ()
showExp f e = do writeFile "exp" $ show e'; drawTerm "exp"
         writeFile "expS" $ showE e'
              where e' = f e
                
loopE :: (Store -> Int) -> IO ()
loopE val = do (store,b) <- input
               when b $ do putStrLn $ "result = "++show (val store)
                       loopE val
       
loopC :: [StackCom] -> IO ()
loopC code = do writeFile "code" $ fold2 f "" [0..] code; loop
         where f str n c = str++'\n':replicate (5-length lab) ' '++
                       lab++": "++show c where lab = show n
           loop = do (store,b) <- input
                     let (result:_,_) = execute code ([],store)
                             when b $ do putStrLn $ "result = "++show result
                             loop 
               
input :: IO (Store,Bool)
input = do putStrLn "Enter variables!"; str <- getLine
           let vars = words str
       putStrLn "Enter values!"; str <- getLine
       let vals = map read $ words str
           vals' = vals++replicate (length vars-length vals) 0
       return (listsToFun 0 vars vals', not $ null vars)

-- SYB

ints :: Expr -> [Int]
ints = everything (++) $ mkQ [] $ \x -> case x of Con i -> [i]
                                  (i :* _) -> [i]
                                  (_ :^ i) -> [i]
                                  _ -> []
sumE :: Expr -> Int
sumE = everything (+) $ mkQ 0 $ \x -> case x of Con i -> i
                                i :* _ -> i
                                _ :^ i -> i
                        _ -> 0

transform :: Expr -> Expr
transform e = everywhere (mkT $ \x -> case x of Con i -> Con $ sumE e; x -> x) e

-- BinŠre BŠume

data Bintree a = Empty | Fork (Bintree a) a (Bintree a)

leaf :: a -> Bintree a
leaf = flip (Fork Empty) Empty
        
bintree :: Compiler a -> Compiler (Bintree a)
bintree comp = do a <- comp
                  msum [do tchar '('; left <- bintree comp
                           msum [do tchar ','; right <- bintree comp
                                    tchar ')'; return $ Fork left a right,
                                 do tchar ')'; return $ Fork left a Empty],
                        return $ leaf a]
             
compileB = runM $ bintree int

instance Show a => Show (Bintree a) where showsPrec _ = showTree

showTree :: Show a => Bintree a -> ShowS
showTree (Fork Empty a Empty) = shows a
showTree (Fork left a Empty)  = shows a . ('(':) . showTree left . (')':)
showTree (Fork left a right)  = shows a . ('(':) . showTree left . (',':) .
                           showTree right . (')':)
showTree _ = const ""
                
-- DefUse
           
data DefUse = Def String Int | Use String
            
updM :: String -> Int -> Trans Store () 
updM x a = T $ \st -> ((),update st x a) 

getM :: String -> Trans Store Int
getM x = T $ \st -> (st x,st) 
           
traceM :: [DefUse] -> [(String,Int)] 
traceM s = fst $ run (f s) $ const 0 where
       f :: [DefUse] -> Trans Store [(String,Int)]
       f (Def x a:s) = do updM x a; f s 
       f (Use x:s)   = do a <- getM x; s <- f s; return $ (x,a):s 
       f _           = return [] 
          
tra = traceM [Def "a" 1,Use "a",Def "b" 2,Use "b",Def "a" 3,Use "a",Use "b"]
      -- > [("a",1),("b",2),("a",3),("b",2)] :: [(String,Int)]
