module Painter where

--------------------------------------- Copyright (c) Peter Padawitz, March 2012

---- Manual: http://fldit-www.cs.uni-dortmund.de/~peter/Haskellprogs/Painter.pdf

import Monad
import Directory

type Pos = (Int,Int)

(f***g) x = (f x,g x)

indices_ s = [0..length s-1]

fold2 :: (a -> b -> c -> a) -> a -> [b] -> [c] -> a
fold2 f a (x:xs) (y:ys) = fold2 f (f a x y) xs ys
fold2 _ a _ _           = a

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d:zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _                     = []

zip4 = zipWith4 $ \a b c d -> (a,b,c,d)

fibs = 0:tailfibs where tailfibs = 1:zipWith (+) fibs tailfibs

float :: RealFloat a => Int -> a
float = fromInteger . toInteger

float2 (x,y) = (float x,float y)

round2 :: (RealFrac b, Integral d, RealFrac a, Integral c) => (a,b) -> (c,d)
round2 (x,y) = (round x,round y)
 
add1 (x,y) a = (x+a,y)
 
add2 (x,y) (a,b) = (x+a,y+b)
 
apply2 f (x,y) = (f x,f y)

-- minmax ps computes minimal and maximal coordinates of the point list ps.

minmax ps@((x,y):_) = foldl f (x,y,x,y) ps
             where f (x1,y1,x2,y2) (x,y) = (min x x1,min y y1,max x x2,max y y2)
minmax _ = (0,0,0,0)

center ps = ((x1+x2)/2,(y1+y2)/2) where (x1,y1,x2,y2) = minmax ps

context i s = take i s++drop (i+1) s

split n s = if length s <= n then [s] else take n s:split n (drop n s)

sort (x:s) = sort [y | y <- s, x <= y]++x:sort [y | y <- s, x > y]
sort s     = s

zipWith2 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith2 f (a:as) (b:bs) (c:cs) = f a b c:zipWith2 f as bs cs
zipWith2 _ _ _ _                = []

max2 :: (Ord a,Num a) => [(a,a)] -> (a,a)
max2 s@(xy:_) = foldl f xy s where f (x,y) (a,b) = (max x a,max y b)
max2 _        = (0,0)

search :: (a -> Bool) -> [a] -> Maybe Int
search f = g 0 where g i (x:s) = if f x then Just i else g (i+1) s
                     g _ _     = Nothing
             
mkSet :: Eq a => [a] -> [a]
mkSet = union []

union :: Eq a => [a] -> [a] -> [a]
union = foldl $ flip insert 

insert x s@(y:s') = if x == y then s else y:insert x s'
insert x _        = [x]

-- READ, WRITE AND DRAW
                    
test :: (Read a,Show b) => (a -> b) -> IO()
test f = readFileAndDo "source" $ writeFile "target" . show . f . read

htmlfile file = "PainterPix/"++file++".html"
svgfile file  = "PainterPix/"++file++".svg"

getFiles dir = do files <- getDirectoryContents dir
          return $ filter ((/= '.') . head) files

movie :: String -> IO ()
movie dir = do files <- getFiles dir
               let (dir0,dir1) = break (== '/') dir
               f file = tail dir1++'/':file
               writeFile (htmlfile dir1) $ html (map f files) $ length files          

prompt1 = "Enter both a horizontal and a vertical scaling factor and - " ++
          "optionally - a background file (gif, jpg, png or svg)!"
                                   
prompt2 = "Enter both a horizontal and a vertical scaling factor!"
                                   
readFileAndDo :: String -> (String -> IO ()) -> IO ()
readFileAndDo file act = do str <- readFile file `catch` const (return "")
                    if null str then putStrLn $ file++" does not exist"
                        else act str

readFileAndDraw :: String -> (Point -> a -> (String,Pos)) -> (String -> a) 
                              -> IO ()
readFileAndDraw file draw parse = readFileAndDo file $ scale . parse
       where scale a = do putStrLn prompt1
                          str <- getLine
                  let strs = words str
                  when (length strs >= 2) $
                       do let [hor,ver] = map read $ take 2 strs
                          rest = drop 2 strs
                      back = if null rest then "" else head rest
                      writeFile (svgfile file) $ svg1 back 
                               $ draw (hor,ver) a
                      scale a

readDirAndDraw :: Read a => String -> Int -> (Point -> a -> (String,Pos)) 
                             -> IO ()
readDirAndDraw dir n draw = do createDirectory $ "PainterPix/"++dir
                   strs <- mapM (readFile . file) [1..n]
                   scale $ map read strs
       where scale as = do putStrLn prompt2
                           str <- getLine
                   let strs = words str
                   when (length strs >= 2) $ do
                        let [hor,ver] = map read $ take 2 strs
                    zipWithM_ write [1..n] $ map (draw (hor,ver)) as
                            scale as
             file i = dir++'/':show i
         write i = writeFile (svgfile $ file i) . svg2

-- drawText file loads a string from file and writes it in columns each of which
-- consists of at most 37 lines into file.svg.

drawText :: String -> IO ()
drawText file = readFileAndDo file act 
            where act str = writeFile (svgfile file) $ svg1 "" (code,size)
               where code = concatMap drawCol [0..nrCols-1]
                 size = (9*pos nrCols+9,20*maximum heights)
                 ls = lines str; nrLines = length ls
                 matrix = split 37 ls; nrCols = length matrix
                 heights = map length matrix
                 pos 0 = 1
                 pos i = pos k+maximum (map length $ matrix!!k)
                             where k = i-1
                     drawCol i = concat $ zipWith (drawLine $ pos i) 
                                   [0..heights!!i-1] $ matrix!!i
                 drawLine p k = text black $ float2 (9*p,24+k*20)
     
-- TREES
                         
data Tree a = F a [Tree a] | V a deriving Show

root :: Tree a -> a
root (F a _) = a

subtrees :: Tree a -> [Tree a]
subtrees (F a ts) = ts

height :: Tree a -> Int
height (F _ ts) = foldl max 0 (map height ts)+1

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (F a ts) = f a $ map (foldT f) ts
             
drawTermW,drawTermCW :: Show b => (a -> b) -> a -> String -> IO ()
drawTermW f a file  = do writeFile file $ show $ f a; drawTerm file
drawTermCW f a file = do writeFile file $ show $ f a; drawTermC file

-- drawTerm{C} file loads an object t from file, compiles it into a tree of type 
-- Tree String, enters a loop that asks for scaling factors and an optional 
-- background and draws t into file.svg. 
-- All subterms of t of type Tree String are not changed by the compilation.
-- In addition, drawTermC colors the nodes at level i with 
-- hue 1 (height t) red i (see section COLORS).

drawTerm,drawTermC :: String -> IO ()
drawTerm file  = readFileAndDraw file drawTree parseDT 
drawTermC file = readFileAndDraw file drawTreeC parseDT 

-- PARSER FROM STRINGS TO TREES

parseDT :: String -> Tree String
parseDT str = case applyP term str of Just (t,_) -> t; _ -> F "no term" []

data Parser a = P {applyP :: String -> Maybe (a,String)}

instance Monad Parser where
         P p >>= f = P $ \str -> do (a,str) <- p str; applyP (f a) str
         return a = P $ \str -> Just (a,str)
     fail _ = mzero

instance MonadPlus Parser where
         mzero = P $ const Nothing
         P p `mplus` P q = P $ \str -> p str `mplus` q str

getChr :: Parser Char
getChr = P $ \str -> do c:str <- Just str; Just (c,str)   

some, many :: Parser a -> Parser [a]
some p = do a <- p; as <- many p; return $ a:as
many p = some p `mplus` return []

sat :: Parser a -> (a -> Bool) -> Parser a
sat p f = do a <- p; guard $ f a; return a

chr, nchr :: Char -> Parser Char
chr  = sat getChr . (==)
nchr = sat getChr . (/=)

string :: String -> Parser String
string = mapM chr

nchars :: String -> Parser Char
nchars = sat getChr . flip notElem

space :: Parser String
space = many $ sat getChr (`elem` " \n\t")

token :: Parser a -> Parser a
token p = do space; a <- p; space; return a
                    
key :: String -> Parser String
key = token . string      

constructor :: Parser String
constructor = token $ msum [quoted,some $ nchars " \n\t:()[]{},="]

quoted :: Parser String
quoted = do chr '"'; x <- many $ nchr '"'; chr '"'; return x

tree = do key "F"; x <- token quoted; key "["
      msum [do key "]"; return $ F x [],
            do ts <- terms; key "]"; return $ F x ts]

term = do t <- singleTerm; moreInfix t
                
singleTerm = msum [tree, do x <- constructor; curryrest $ F x [],
                         do key "("; u <- tuple; curryrest u,
                 do key "["; u <- list; curryrest u]

moreInfix t = msum [do space; chr ':'; x <- constructor; space; u <- term
               return $ F (':':x) [t,u],
                return t]

curryrest t = msum [tree, do x <- constructor; curryrest $ mkF $ F x [],
                          do key "("; u <- tuple; curryrest $ mkF u,
              do key "["; u <- list; curryrest $ mkF u,
              do key "{"; u <- set; return $ mkF u,
              return t]
               where mkF u = if null $ subtrees t 
                     then F (root t) $ if root u `elem` words "() [] {}"
                                   then subtrees u else [u] 
                     else F "$" [t,u]

terms = do t <- term; msum [do key ","; ts <- terms; return $ t:ts, return [t]]

tuple = msum [do key ")"; return $ F "()" [],
          do ts <- terms; key ")"; return $ case ts of [t] -> t
                                   _ -> F "()" ts]

list = msum [do key "]"; return $ F "[]" [],
         do ts <- terms; key "]"; return $ F "[]" ts]

set = msum [do key "}"; return $ F "{}" [],
        do ts <- equations; key "}"; return $ F "{}" ts]
        
equations = do at <- token $ some $ nchars " ="; key "="; t <- term
           msum [do key ","; ts <- equations; return $ F at [t]:ts, 
                 return [F at [t]]]
             
-- TREES WITH NODE POSITIONS

type PTree = Tree (String,Point)
     
lgW = float . (5*) . length
                  
textblock :: Point -> String -> ([String],[Point],Float,Float)
textblock (x,y) a = (ls,map f [0..upb],maximum $ map lgW ls,h)
                    where ls = lines a [] ""
                  lines ('|':a) ls b = lines a (ls++[b]) ""
              lines (x:a) ls b   = lines a ls $ b++[x]
              lines _ ls b       = ls++[b]
              f i = (x,y-h+20*float i)
              h = 10*float upb
              upb = length ls-1

rxNode :: Bool -> String -> Float
rxNode c a = if c then w else lgW a where (_,_,w,_) = textblock p0 a

treeSize :: Bool -> PTree -> Pos
treeSize c = round2 . max2 . foldT bounds
             where bounds :: (String,Point) -> [[Point]] -> [Point]
           bounds (a,p) pss = add1 p (-x):add1 p x:concat pss 
                              where x = rxNode c a

-- drawTree{C} spread t computes SVG code for the tree of type PTree each of 
-- whose node labels includes the position where the node is to be placed.

drawTree,drawTreeC :: Point -> Tree String -> (String,Pos)
drawTree spread t = (f pt,treeSize True pt)
     where pt = mkPTree True spread (20,20) t
           f :: PTree -> String
           f (F (a,p) pts) = g pts++concat lines
                     where g (pt@(F (_,q) _):pts) = line red p q++f pt++g pts
                           g _ = ""
               lines = zipWith (drawLNode rectangle yellow) ps ls
               (ls,ps,_,_) = textblock p a
drawTreeC spread t = (f red pt,treeSize False pt)
          where pt = mkPTree False spread (20,20) t
            f :: RGB -> PTree -> String
            f col (F (a,p) pts) = g (nextColor 1 (height t) col) pts ++
                                  drawLNode ellipse (light col) p a
                          where g col (pt@(F (_,q) _):pts) = line col p q++
                                     f col pt++g col pts
                        g _ _ = ""
            
treeArc :: RGB -> Point -> Point -> String          -- not used
treeArc col p@(x,y) q@(x',y') = if x == x' then line col p q
                           else polyline True col [p,(x',y),q]

-- mkPTree b p hor ver t adds positions in the plane to the nodes of t such that
-- p is the leftmost-uppermost corner of the smallest rectangle enclosing t. hor 
-- and ver are the horizontal resp. vertical spaces between adjacent nodes. 
-- mkPTree True admits multiline node labels and interprets '|' as the line 
-- break symbol

mkPTree :: Bool -> Point -> Point -> Tree String -> PTree
mkPTree c _ p (F a [])                    = F (a,add1 p $ rxNode c a) []
mkPTree c spread@(hor,ver) (x,y) (F a ts) = 
        if diff <= 0 then pt' else translate (diff/2) pt'
            where diff = rxNode c a-b
          pt' = F (a,((g hd+g (last pts'))/2,y)) pts'
              (pts'@(hd:_),b) = translateT c hor [pt] (foldT f pt-x) pts
          pt:pts = map (mkPTree c spread (x,y+ver+incr)) ts
          f (a,(x,_)) = maximum . (x+rxNode c a:)
          g (F (_,(x,_)) _) = x
          incr = if c then hdiff/2 else 0
          hdiff = height a+maximum (map (height . root) ts)
              height a = h+h where (_,_,_,h) = textblock p0 a
            
-- translate x pt moves pt by x units to the right.

translate :: Float -> PTree -> PTree
translate x (F (a,p) ts) = F (a,add1 p x) $ map (translate x) ts

-- translateT hor ts0 b0 ts orders the trees of ts0++ts horizontally with a 
-- horizontal space of 5 units between adjacent trees. translateT takes into
-- account different heights of adjacent trees by shifting the trees to the left
-- or right such that parts of a tree may occur below an adjacent one.
-- offset ts n computes the offset by which the trees of ts must be shifted
-- in order to avoid that they overlap a neighbour with n margins on the left.

translateT :: Bool -> Float -> [PTree] -> Float -> [PTree] -> ([PTree],Float)
translateT c hor = f
   where f ts0 b0 (t:ts) = if b < 0 then f (map (translate (-b)) ts0++[t]) b0 ts
                        else f (ts0++[translate b t]) (b0+b) $ 
                           map (translate b) ts
                       where b = offset ts0+hor
                             mht = min $ height t
                     left = flip (g (-) minimum) t
                 offset = maximum . (float minBound:) . map f
                                      where f t = right n t-left n 
                                  where n = mht $ height t
         f ts b _ = (ts,b)
     right = g (+) maximum
     g op m n (F (a,(x,_)) ts) = if n == 1 then w
                               else m $ w:map (g op m $ n-1) ts
                     where w = op x $ rxNode c a

-- GRAPHS

type Point  = (Float,Float)
type LPoint = (String,Float,Float)
type Path   = [Point]
type LPath  = [LPoint]
type CPath  = (Path,RGB,Int,Point)
type LCPath = (LPath,RGB)

-- straight p q r checks whether p, q and r form a line

straight :: Point -> Point -> Point -> Bool
straight (x1,y1) (x2,y2) (x3,y3) = x1 == x2 && x2 == x3 || 
                                   x1 /= x2 && x2 /= x3 &&
                                   (y2-y1)/(x2-x1) == (y3-y2)/(x3-x2)

minimize :: Path -> Path
minimize (p:ps@(q:r:s)) = if straight p q r then minimize $ p:r:s
                                    else p:minimize ps
minimize ps = ps  

-- draw{L}Graph loads a graph g of type [Path] resp. [LPath] from a file, 
-- enter a loop that asks for scaling factors and an optional background and 
-- draw g into file.svg.

-- If color c has been assigned to path p, draw{L}Graph colors the i-th node of 
-- p with hue 1 (length p) c i (see section COLORS).

drawGraph :: String -> IO ()
drawGraph file = readFileAndDraw file svgcode read
       
svgcode :: Point -> [CPath] -> (String,Pos)
svgcode (hor,ver) paths = (code,size)
                   where (minx,miny,maxx,maxy) = minmax $ concatMap coords paths
                 coords :: CPath -> Path
             coords ((a,b):_,_,0,(x,y)) = [(x-a,y-b),(x+a,y+b)]
             coords (ps,_,_,_)      = ps
                 code = concatMap (drawP . shift) paths
                     size = round2 $ add1 (g (maxx,maxy)) 20
             shift ((a,b):ps,col,0,p) = ((a*hor,b*ver):ps,col,0,g p) 
             shift (ps,col,mode,p) = (map g ps,col,mode,g p) 
             g (x,y) = ((x-minx)*hor+40,(y-miny)*ver+20)

drawP :: CPath -> String
drawP (p:_,col,0,r)          = ellipse col r p
drawP (ps@(p:qs),col,mode,r) = edgesCode++nodesCode
    where is = indices_ ps
      mkModes str = if lg < 5 then str++replicate (5-lg) '1' else take 5 str
                where lg = length str  
      [nodeM,edgeM,smooth,colM,gradM] = mkModes $ show mode
          nodesCode = case nodeM of '1' -> ""
                                '2' -> nodes $ \c p -> ellipse c p (12,12)
                                    '3' -> nodes $ \c p -> rectangle c p (12,12)
                    _ -> drawLNodes c ps $ map show is
          edgesCode = case edgeM of '1' -> ""
                                    '2' -> edges False
                    '3' -> edges True
                    '4' -> polyline b col ps
                    _ -> polygon b col ps
      b = smooth > '1'
      m = if b then if head ps == last ps then 2 else 1 else 0
      c = cols col colM lg
      (ks,lg) = inds ps is gradM
      nodes code = concat $ zipWith (code . mkLight . c) ks ps
      edges tria = fst $ fold2 g ("",p) qs ks
            where g (code,p) q i = (code++code',q)
                        where code' = if tria then polygon b (c i) [r,p,q,r]
                                              else line (c i) p q
                           
cols :: RGB -> Char -> Int -> Int -> RGB
cols col m lg i = if m `elem` "123" then hue (read [m]) lg col i else col
                           
inds :: Path -> [Int] -> Char -> ([Int],Int)
inds ps@(p:qs) is gradM = case gradM of '1' -> (is,lg)
                    '2' -> pair
                    '3' -> h $ (round .) . angle
                    _   -> h slope
                   where lg = length ps-1; lg2 = lg`div`2; half = [0..lg2-1]
                 pair = if lg`mod`2 == 0 then (half++reverse half,lg2)
                    else (half++lg2:reverse half,lg2+1)
                 h rel = (map g rels,length set)
                       where f (is,p) q = (is++[rel p q],q)
                     rels = fst $ foldl f ([],p) qs
                 set = sort $ mkSet rels
                     g rel = case search (== rel) set of Just i -> i
                                     _ -> 0

drawLGraph :: String -> Int -> IO ()
drawLGraph file mode = readFileAndDraw file svgcode read
     where svgcode :: Point -> [LCPath] -> (String,Pos)
       svgcode (hor,ver) paths = (code,size)
                   where (minx,miny,maxx,maxy) = minmax $ concatMap coords paths
                     code = concatMap (drawLP mode . shift) paths
             size = round2 $ add1 (f (maxx,maxy)) 20
             shift (ps,col) = ([(lab,x',y') | (lab,x,y) <- ps, 
                                        let (x',y') = f (x,y)], col)
             f (x,y) = ((x-minx)*hor+40,(y-miny)*ver+20)
           coords :: LCPath -> Path
       coords (lpath,_) = map (\(_,x,y) -> (x,y)) lpath

drawLP :: Int -> LCPath -> String
drawLP mode (lpath,col) = polyline False col ps ++ drawLNodes c ps labs
              where (ps,labs) = unzip $ map f lpath
                        f (lab,p,q) = ((p,q),lab)
                    c = cols col (head $ show mode) $ length ps

-- GRAPHIC PRIMITIVES

angle :: RealFloat a => (a,a) -> (a,a) -> a
angle (x1,y1) (x2,y2) = f (y2-y1) (x2-x1)*180/pi where f 0 0 = atan2 0 1
                                   f x y = atan2 x y

slope (x1,y1) (x2,y2) = if y1 == y2 then 0
                            else if abs (x1-x2) < 0.1 
                         then 99999999 else (y2-y1)/(x2-x1) 

sincos a = (sin deg,cos deg) where deg = a*pi/180
                  
-- successor p (distance p q) (angle p q) = q. 

successor :: Floating a => (a,a) -> a -> a -> (a,a)
successor (x,y) r a = (x+r*c,y+r*s) where (s,c) = sincos a

distance :: Floating a => (a,a) -> (a,a) -> a
distance (x1,y1) (x2,y2) = sqrt $ (x2-x1)^2+(y2-y1)^2

perimeter :: Path -> Float
perimeter ps = if peri <= 0 then 0.01 else peri
               where peri = sum $ zipWith distance ps $ tail ps

        
addPoints :: Path -> [Float] -> Path
addPoints ps []               = ps
addPoints (p:ps@(q:_)) (d:ds) = if d > d' then p:addPoints ps (d-d':ds)
                        else p:addPoints (successor p d a:ps) ds
                        where d' = distance p q; a = angle p q
addPoints _ _ = error "addPoints"
    
adaptLength :: Int -> Path -> Path
adaptLength n ps = if n > 0 then addPoints ps $ dps/2:replicate (n-1) dps
                            else ps
                   where dps = perimeter ps/k; k = float n

-- rotate q a p rotates p clockwise by a around q on the axis (0,0,1).

rotate :: Point -> Float -> Point -> Point
rotate _ 0 p             = p -- sincos 0 = (0,1)
rotate q@(i,j) a p@(x,y) = if p == q then p else (c*x1-s*y1+i,s*x1+c*y1+j)
               where (s,c) = sincos a; x1 = x-i; y1 = y-j
                      
-- spline0 b ps uses ps as control points for constructing a closed (b = True) 
-- resp. open (b = False) B-spline with degree 3; see Paul Burke, Spline Curves
-- (http://astronomy.swin.edu.au/~pbourke/curves/spline)
-- or Heinrich MŸller, B-Spline-Technik, Vorlesung Geometrisches Modellieren 
-- (http://ls7-www.cs.tu-dortmund.de).

spline :: [Point] -> [Point]
spline ps = if head ps == last ps then spline0 True $ init ps
                  else spline0 False ps
  where spline0 b ps = first:map f [1..resolution]++map g [1..9]++
                   [if b then first else ps!!(n-1)]
         where first = f 0; n = length ps; resolution = n*6
           f i = point $ upb*float i/float resolution 
               g i = point $ upb+float i/10
           upb = float n-if b then 1 else 3
           point v = foldl1 add2 $ map h [0..n-1]
                     where h i = apply2 (*z) $ ps!!i
                             where z | b && v < u i = blend2 u i $ v-u 0+u n
                                 | b            = blend2 u i v
                                 | True         = blend2 t i v 
               t i = if i < 3 then 0 else float (min i n)-2
           u i = if i <= n then float i else u (i-1)+u (i-n)-u (i-n-1)
           blend2 t i v = case (denom1,denom2) of (0,0) -> 0
                                  (0,_) -> sum2
                              (_,0) -> sum1
                              _     -> sum1+sum2
                      where ti = t i; ti3 = t $ i+3
                            denom1 = t (i+2)-ti;  num1 = v-ti
                    denom2 = ti3-t (i+1); num2 = ti3-v
                    sum1 = num1/denom1*blend1 t i v
                    sum2 = num2/denom2*blend1 t (i+1) v
               blend1 t i v = case (denom1,denom2) of (0,0) -> 0
                                  (0,_) -> sum2
                                          (_,0) -> sum1
                              _     -> sum1+sum2
                      where ti = t i; ti1 = t $ i+1; ti2 = t $ i+2
                            denom1 = ti1-ti;  num1 = v-ti 
                    denom2 = ti2-ti1; num2 = ti2-v
                    sum1 = if b i then num1/denom1 else 0
                    sum2 = if b $ i+1 then num2/denom2 else 0
                    b i = t i <= v && v < t (i+1)

-- CURVES

data Curves = C {file :: String, paths :: [Path], colors :: [RGB], 
         modes :: [Int], points :: [Point]} 
        
drawC :: Curves -> IO ()
drawC (C f paths cs ms ps) = do writeFile f $ show $ zip4 paths cs ms ps
                            drawGraph f

drawCS :: String -> [Curves] -> IO ()
drawCS dir cs = do createDirectory dir
           zipWithM_ (write . file) [1..n] cs
           let files = map file' [1..n]
           writeFile (htmlfile dir) $ html files n
               readDirAndDraw dir n svgcode
                where n = length cs
              file i = dir++'/':show i
              file' i = file i++".svg"
                  write f (C _ paths cs ms ps) = 
                                    writeFile f $ show $ zip4 paths cs ms ps

-- Curve modifiers and combinators

combine,overlay :: [Curves] -> Curves
combine cs@(c:_) = c {paths = f paths, colors = f colors,
                      modes = f modes, points = f points} 
                   where f = flip concatMap cs
overlay = combine . map toCenter
        
mapCurves :: (Point -> Point) -> Curves -> Curves
mapCurves f c = c {paths = map (map f) $ paths c, points = map f $ points c}

morphing :: Int -> Int -> [Curves] -> Curves
morphing m n = combine . morphs m n

morphs :: Int -> Int -> [Curves] -> [Curves]
morphs m n cs = concat $ zipWith f (init cs) $ tail cs
 where f c d = map g [0..n]
           where g i = c {paths = zipWith h (paths c) $ paths d,
                      colors = map hc $ colors c,
                              points = h (points c) $ points d}
                       where h ps qs = zipWith morph ps' qs'
                                   where diff = length ps-length qs
                             ps' = adaptLength (-diff) ps
                             qs' = adaptLength diff qs
                     morph (xc,yc) (xd,yd) = (next xc xd,next yc yd)
                     next x z = (1-inc)*x+inc*z
                     inc = float i/float n
                 hc col = if m `elem` [1,2,3] then hue m n col i
                                      else col

rainbow :: Int -> Int -> Curves -> Curves
rainbow m n c = combine $ f c n 
            where f _ 0 = []
              f c i = scale (float i/float n) c:f c' (i-1)
                  where c' = c {colors = map g $ colors c}
              g c = if m `elem` [1,2,3] then nextColor m n c else c
                
shift :: Point -> Curves -> Curves
shift (a,b) c = c {paths = zipWith f (modes c) $ paths c, 
           points = map g $ points c}
            where f 0 = id
              f _ = map g
              g (x,y) = (x+a,y+b)

scale,hscale,vscale,turn :: Float -> Curves -> Curves
scale a c  = c {paths = zipWith3 f ps qs $ paths c, points = qs} 
         where f (b,c) (d,e) = map $ \(x,y) -> (a*(x-b)+d,a*(y-c)+e)
                   ps = points c; qs = map g ps
           g (x,y) = (a*x,a*y)
hscale a c = c {paths = zipWith3 f ps qs $ paths c, points = qs} 
         where f (b,_) (d,_) = map $ \(x,y) -> (a*(x-b)+d,y)
                   ps = points c; qs = map g ps
           g (x,y) = (a*x,y)
vscale a c = c {paths = zipWith3 f ps qs $ paths c, points = qs} 
         where f (_,c) (_,e) = map $ \(x,y) -> (x,a*(y-c)+e)
                   ps = points c; qs = map g ps
           g (x,y) = (x,a*y)
turn a c   = c {paths = zipWith3 f (modes c) (points c) $ paths c} 
         where f 0 _ = id
               f _ q = map $ \p -> rotate q a p
        
updCol :: RGB -> Curves -> Curves
updCol col c  = c {colors = take (length $ colors c) $ repeat col}

hueCol :: Int -> RGB -> Curves -> Curves
hueCol m col c = c {colors = map (hue m (length ps) col) $ indices_ ps}
             where ps = colors c

updMod,shiftCol,takeC,dropC :: Int -> Curves -> Curves
updMod mode c = c {modes = take (length $ modes c) $ repeat mode}
shiftCol n c  = c {colors = map ((!!n) . iterate nextCol) $ colors c}
takeC n c     = c {paths = map (take n) $ paths c}
dropC n c     = c {paths = map (drop n) $ paths c}

complCol,transpose,mkCenter,toCenter,flipH,flipV :: Curves -> Curves
complCol   = shiftCol 765
transpose  = mapCurves $ \(x,y) -> (y,x)
mkCenter c = c {points = take (length ps) $ repeat $ center ps} 
         where ps = points c
toCenter c = c {paths = zipWith f (points c) ps,
        points = take (length ps) $ repeat p0} 
         where ps = paths c
               f (a,b) = map $ \(x,y) -> (x-a,y-b)
flipH      = mapCurves (\(x,y) -> (x,-y)) . toCenter
flipV      = mapCurves (\(x,y) -> (-x,y)) . toCenter

data Action = Turn Float | Move Float

executeActs :: [Action] -> Path
executeActs = fst . foldl f ([p0],0)
          where f (ps,a) (Move d) = (ps++[successor (last ps) d a],a)
                    f (ps,a) (Turn b) = (ps,a+b)

-- Curve generators

p0 = (0,0)

emptyC   = C "" [] [] [] []

elli a b = C "elli" [[(a,b)]] [red] [0] [p0]

tria,circ :: Int -> Float -> Curves
tria mode r = turn 30 $ poly mode 3 [r]
circ mode r = poly mode 100 [r]

rect :: Int -> Point -> Curves
rect mode (b,h) = C "rect" [[p,(b,-h),(b,h),(-b,h),p]] [red] [mode] [p0]
          where p = (-b,-h)
        
poly :: Int -> Int -> [Float] -> Curves
poly mode n rs = if k <= 1 then emptyC
               else C "poly" [last ps:ps] [red] [mode] [p0]
             where k = n*length rs; inc = 360/float k
               ps = fst $ foldl f ([],0) $ take k $ cycle rs
               f (ps,a) 0 = (ps,a+inc)
               f (ps,a) r = (successor p0 r a:ps,a+inc)

snow :: Int -> Int -> Float -> Int -> Curves -> Curves
snow m n r k c = if n <= 0 then emptyC
         else combine $ scale r c:f (nextColor 1 n col) (n-1) r [p0]
       where col = head $ colors c
             f _ 0 _ _    = []
             f col i r ps = zipWith2 (new col r') (bits m) (angs m) qs ++
                    f (nextColor 1 n col) (i-1) r' qs
               where r' = r/3; qs = concatMap circle ps 
                     circle p@(x,y) = if odd m then s else p:s
                      where s = take k $ iterate (rotate p a) (x,y-r+r')
         new col r b a p = shift p $ updCol col $ scale (b*r) $ turn a c
         zeros = 0:zeros; ones = 1:ones; blink = 1: -1:blink
         bits 1 = blink
         bits 2 = 1:take k blink++bits 2
         bits m = ones
         a = 360/float k
         iter = take k $ iterate (+a) 0
         angs m = if m < 5 then zeros else if m == 5 then iter++angs m 
                  else 0:iter++concatMap s [1..n-2]
              where s i = concatMap (f i) iter 
                    f 1 a = a:iter
                f i a = f k a++s k where k = i-1
                       
data Direction = North | East | South | West

move :: Direction -> [Action]
move dir = case dir of North -> [Turn (-90),Move 1,Turn 90]; East -> [Move 1]
                       South -> [Turn 90,Move 1,Turn (-90)]; West -> [Move (-1)]

swap dir = case dir of North -> West; East -> South; South -> East; _ -> North
            
flip' dir = case dir of North -> South; East -> West; South -> North; _ -> East 

mkCurve file ps mode = C file [ps] [red] [mode] [center ps]
        
hilb,cant,snake,phyllo :: Int -> Int -> Curves
hilb mode n = mkCurve "hilb" (executeActs $ acts n East) mode
          where acts :: Int -> Direction -> [Action]
            acts 0 _   = []
            acts n dir = f sdir++move dir++f dir++move sdir++
                                 f dir++move (flip' dir)++f (flip' sdir)
                     where f = acts $ n-1; sdir = swap dir
cant mode n = mkCurve "cant" (map (float2 . p) [0..n*n-1]) mode
          where p 0 = (0,0)
                p i = if even x 
                  then if even y
                   then if x > 0 
                        then if y' < n then (x-1,y') else (x',y)
                        else if y' < n then (0,y') else (1,y) 
                   else if x' < n then (x',y-1) else (x,y') 
                  else if even y
                   then if y > 0 
                        then if x' < n then (x',y-1) else (x,y') 
                    else if x' < n then (x',0) else (x,1)
                   else if y' < n then (x-1,y') else (x',y) 
                  where (x,y) = p $ i-1; x' = x+1; y' = y+1
snake mode n  = mkCurve "snake" (map (float2 . p) [0..n*n-1]) mode
            where p i = (if even yi then xi else n-1-xi,yi)
                where xi = i`mod`n; yi = i`div`n
phyllo mode n = mkCurve "phyllo" (map f [1..n]) mode
        where f i = successor p0 (sqrt k) $ k*137.50776405003785
                where k = float i

fibo,kpath,ktour :: Int -> Int -> Float -> Float -> Curves
fibo mode n a b  = mkCurve "fibo" ps mode
           where ps = executeActs $ Move 1:
                  concatMap f (take n $ tail $ concat fibs)
                         f 0 = [Turn a,Move 1]
             f _ = [Turn (-b),Move 1]
             fibs = [1]:tailfibs
             tailfibs = [0]:zipWith (++) tailfibs fibs
kpath mode n a b = case pathloop n (a,b) of 
            Just ps -> mkCurve "kpath" ps mode; _ -> emptyC
ktour mode n a b = case tourloop n (n*n) [p] [nextPoss n [] p] of 
                Just ps -> mkCurve "ktour" ps mode; _ -> emptyC
           where p = (a,b)

-- knight paths (Springer-Wege): iterative version

pathloop :: Int -> Point -> Maybe Path
pathloop n p = f (n*n) [p] [nextPoss n [] p]
       where f 1 ps _            = Just $ reverse ps
             f k ps ((p:qs):pss) = if p `elem` ps then f k ps qss
                           else f (k-1) (p:ps) $ nextPoss n ps p:qss
                           where qss = qs:pss
                 f k ps pss          = do _:ps <- Just ps; _:pss <- Just pss
                      f (k+1) ps pss

nextPoss,newSucs :: Int -> Path -> Point -> Path
nextPoss n = sortVal . newSucs n
newSucs n visited = filter (`notElem` visited) . sucs n

sucs :: Int -> Point -> Path
sucs n p = [q | q@(a,b) <- incrs p, 0 < a, a <= float n, 0 < b, b <= float n]
            
incrs :: Point -> Path
incrs (x,y) = [(x+1,y+2),(x+1,y-2),(x-1,y+2),(x-1,y-2),
           (x+2,y+1),(x+2,y-1),(x-2,y+1),(x-2,y-1)]
          --[(x+1,y),(x,y-1),(x-1,y),(x-1,y),
          -- (x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)]
            
sortVal :: (Point -> Path) -> Point -> Path
sortVal f p = sort $ f p
   where sort (x:s) = sort [y | y <- s, r y x]++x:sort [y | y <- s, not $ r x y]
         sort s     = s
         r p q = length (f p) < length (f q)

-- knight tours (Springer-Kreise)

tourloop :: Int -> Int -> Path -> [Path] -> Maybe Path
tourloop n 1 ps@(p:qs) (_:pss) = if q `elem` sucs n p then Just $ reverse $ q:ps 
                              else tourloop n 2 qs pss
                         where q = last ps            -- = init
tourloop n k ps ((p:qs):pss)   = if p `elem` ps then tourloop n k ps qss
                         else tourloop n (k-1) (p:ps) $
                                           nextPosst n ps (last ps) p:qss
                     where qss = qs:pss           -- last ps = init
tourloop n k ps pss            = do _:ps <- Just ps; _:pss <- Just pss
                    tourloop n (k+1) ps pss

nextPosst :: Int -> Path -> Point -> Point -> Path
nextPosst n visited init p
       | init `notElem` sucs n p = case singlesuc fp of
                               Just i -> case singlesuc $ context i fp of
                                 Just _ -> []           -- (c)
                         _ -> [fp!!i]       -- (b)
                                   _ -> sortVal f p         -- (d)
       | null $ f init           = []                           -- (a)
       | True                = sortVal f p                  -- (d)
                           where f = newSucs n visited
                             fp = f p
                         singlesuc = search ((==1) . length . f)

       -- (a)-(d) implement the synonymous rules of 
       -- Rabhi, Lapalme, Algorithms, Addison-Wesley 1999, Exercise 8.7.
 
-- COLORS

data RGB = RGB Int Int Int deriving Eq

red     = RGB 255 0 0
green   = RGB 0 255 0
blue    = RGB 0 0 255
yellow  = RGB 255 255 0 
magenta = RGB 255 0 255
cyan    = RGB 0 255 255
orange  = RGB 255 180 0
brown   = RGB 0 160 255
black   = RGB 0 0 0
white   = RGB 255 255 255
grey    = RGB 100 100 100

light (RGB x y z) = RGB (f x) (f y) (f z) where f x = x+(255-x)`div`2 

dark (RGB x y z)  = RGB (f x) (f y) (f z) where f x = x+x`div`2 
              
mkLight :: RGB -> RGB
mkLight c = if c == black then white else light c

-- nextCol computes the successor of each color c c on a chromatic circle of 
-- 1530 equidistant pure (or hue) colors. A color c is pure if c is neither
-- black nor white and at most one of the R-, G- and B-values of c is different
-- from 0 and 255.

nextCol,complColor :: RGB -> RGB
 
nextCol (RGB 255 n 0) | n > 0   = RGB 255 (n-1) 0        -- n = 255 --> yellow
nextCol (RGB 255 0 n) | n < 255 = RGB 255 0 (n+1)        -- n = 0   --> red
nextCol (RGB n 0 255) | n > 0   = RGB (n-1) 0 255        -- n = 255 --> magenta
nextCol (RGB 0 n 255) | n < 255 = RGB 0 (n+1) 255        -- n = 0   --> blue 
nextCol (RGB 0 255 n) | n > 0   = RGB 0 255 (n-1)        -- n = 255 --> cyan 
nextCol (RGB n 255 0) | n < 255 = RGB (n+1) 255 0        -- n = 0   --> green
nextCol c           = c 
 
complColor c = iterate nextCol c!!765
  
-- hue m n c i computes the i-th successor of c in a chromatic circle of 
-- n <= 1530 pure colors.

hue :: Int -> Int -> RGB -> Int -> RGB
hue 1 n c i = iterate nextCol c!!round (float i*1530/float n) 
hue 2 n c 0 = c
hue 2 n c i = if odd i then complColor $ hue 2 n c $ i-1
                       else nextColor 1 (n `div` 2) $ hue 2 n c $ i-2
hue 3 n c i = if odd i then complColor d else d where d = hue 1 n c i

-- nextColor m n c computes the direct successor of c in a chromatic circle of 
-- n <= 1530 pure colors.

nextColor :: Int -> Int -> RGB -> RGB
nextColor m n c  = hue m n c 1

instance Read RGB where 
              readsPrec _ s1 = 
                    [(RGB r g b,s5) | ("RGB",s2) <- lex s1,(r,s3) <- reads s2,
                                  (g,s4) <- reads s3, (b,s5) <- reads s4] ++
                    [(rgb c,s2) | (c,s2) <- lex s1, c `elem` 
                         words "red green blue yellow magenta cyan black white"]
          
rgb "red"    = red;    rgb "green"   = green;   rgb "blue" = blue
rgb "yellow" = yellow; rgb "magenta" = magenta; rgb "cyan" = cyan
rgb "black"  = black;  rgb "white"   = white

instance Show RGB where 
              showsPrec _ (RGB x y z) = ("RGB "++) . shows x . (' ':) . shows y
                                     . (' ':) . shows z

-- HTML- und SVG-Code

infixl 6 &,%

(&) :: String -> String -> String
x & val = x ++ "=\"" ++ val ++ "\" "

(%) :: Show a => String -> a -> String
x % val = x & show val

html :: [String] -> Int -> String
html files n = 
    "<html>\n<head>\n<script type"&"text/javascript" ++ 
    ">\nvar images = new Array(" ++ '\"' : first ++ "\"" ++ 
    concatMap f (tail files) ++ 
    ")\n</script>\n<script type"&"text/javascript" ++ "src"&"Painter.js" ++ 
    ">\n</script>\n</head>\n<body style"&"background-color: rgb(221,221,255)" ++ 
    ">\n<img id"&"img" ++ "src"&first ++ "width"&"500" ++ "height"&"500" ++ 
    ">\n<input type"&"button" ++ "value"&"go" ++ "onclick"&"pushGo(this)" ++ 
    ">\n<br>\n<input id"&"fileSlider" ++ "type"&"range" ++ "min"&"0" ++ 
    "max"%(n-1) ++ "value"&"0" ++ 
    "onchange"&"showFile(this.value); counter=this.value" ++ ">\n<span id"&
    "file" ++ '>' : first ++ "</span>\n<input type"&"button" ++ "value"&"set" ++ 
    "onclick"&"push()" ++ ">\n<br>\n<input type"&"range" ++ "min"&"0" ++ 
    "max"&"2000" ++ "value"&"1000" ++ "onchange"&"showTime(this.value)" ++ 
    ">\n<span id"&"time" ++ ">1000</span> millisecs\n</body>\n</html>"
    where first = head $ files
          f file = ",\"" ++ file ++ "\""

svgHead = "<svg xmlns"&"http://www.w3.org/2000/svg" ++
      "\nxmlns:xlink"&"http://www.w3.org/1999/xlink" ++
      "\nversion"&"1.1" ++ "baseProfile"&"full"
                
svg1 :: String -> (String,Pos) -> String
svg1 back (code,(x,y)) = svgHead ++ "\nwidth"%(x+20) ++ "height"%(y+20) ++ '>':
                 (if null back then "" 
                           else "\n<image xlink:href=\""++ back ++
                                '\"' : frame ++ "/>") ++
                 "\n<rect" ++ frame ++ "fill"&"none" ++ 
                              "stroke"&"rgb(100,100,100)" ++ 
                          "stroke-width"&"1.5" ++ "/>" ++ code ++ 
                 "</svg>"
                         where frame = " x"&"5" ++ "y"&"5" ++
                                       "width"%(x+10) ++ "height"%(y+10)
                
svg2 :: (String,Pos) -> String
svg2 (code,(x,y)) = svgHead ++ "\nwidth"%(x+20) ++ "height"%(y+20) ++ '>': 
                    code ++ "</svg>"

rectangle :: RGB -> Point -> Point -> String
rectangle col (x,y) (rx,ry) = 
            polygon False col [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]
                    where x1 = x-rx; x2 = x+rx; y1 = y-ry; y2 = y+ry
            
drawLNode :: (RGB -> Point -> Point -> String) -> RGB -> Point -> String 
                                   -> String
drawLNode f col p@(x,y) a = if null a then "" else f col p (width+2,12) ++
                           text black (x-width,y+5) a
                            where width = lgW a

drawLNodes :: (Int -> RGB) -> Path -> [String] -> String
drawLNodes c ps = concat . zipWith2 (drawLNode ellipse . mkLight . c) [0..] ps

ellipse :: RGB -> Point -> Point -> String
ellipse (RGB r g b) (x,y) (rx,ry) = "\n<ellipse cx"%round x ++ "cy"%round y ++ 
                               "rx"%round rx ++ "ry"%round ry ++ 
                           "fill"&("rgb"++show (r,g,b)) ++
                    "/>"

line :: RGB -> Point -> Point -> String
line (RGB r g b) (x1,y1) (x2,y2) = "\n<line x1"%round x1 ++ "y1"%round y1 ++ 
                           "x2"%round x2 ++ "y2"%round y2 ++ 
                       "stroke"&("rgb"++show (r,g,b)) ++
                       "stroke-width"&"1.5" ++ "/>"

polyline :: Bool -> RGB -> [Point] -> String
polyline smooth (RGB r g b) ps = if null ps then ""
                     else "\n<polyline points"&
                      concatMap (f . round2) qs ++
                      "stroke"&("rgb"++show (r,g,b)) ++
                      "stroke-width"&"1.5" ++ 
                      "fill"&"none" ++ "/>"
                         where f (x,y) = ' ':show x++',':show y
                       qs = if smooth then spline ps else ps

polygon :: Bool -> RGB -> [Point] -> String
polygon smooth (RGB r g b) ps = if null ps then ""
                        else "\n<polygon points"&
                     concatMap (f . round2) qs ++ 
                     "fill"&("rgb"++show (r,g,b)) ++ "/>"
                        where f (x,y) = ' ':show x++',':show y
                          qs = if smooth then spline ps else ps

text :: RGB -> Point -> String -> String
text (RGB r g b) (x,y) a = "\n<text x"%round x ++ "y"%round y ++ 
               "font-family"&"courier" ++ "font-size"&"16" ++ 
               "fill"&("rgb"++show (r,g,b)) ++ '>':a ++ "</text>"

               
-- EXAMPLES

fp1 = drawLGraph "fahrplan" 2   -- hor=4, ver=14

hilb1  = hilb 12111 5
hilb2  = hilb 21111 4       -- hor=ver=24
hilb3  = hilb 31111 4       
hilb4  = hilb 31121 4       
hilb5  = hilb 14211 4       
hilb6  = hilb 13111 4       
hilb7  = hilb 42111 4

hilb8  = hilb 22121 4       -- hor=ver=33
hilb9  = hilb 12112 4       
hilb10 = hilb 12113 4       
hilb11 = hilb 12114 4       
hilb12 = flipV $ hilb1
hilb13 = turn 22 $ hilb1
hilb14 = overlay [hilb1,hilb2]

cant1 = cant 12111 33
cant2 = flipV cant1
cant3 = overlay [cant1,cant2]
cant4 = cant 12121 33
cant5 = cant 13111 33
cant6 = cant 13113 33
cant7 = cant 13114 33

snake1 = snake 12111 33
snake2 = transpose snake1
snake3 = overlay [snake1,snake2]
snake4 = snake 122111 33

cs1 = overlay [cant3,snake1]
cs2 = overlay [cant1,snake3]
cs3 = overlay [cant3,snake3]    -- hor=ver=15
cs4 = updMod 12121 cs3      

-- see http://www.win.tue.nl/~hzantema/strfib.html

fibo1 = flipH $ fibo10 21111 
fibo2 = flipH $ fibo10 31111
fibo3 = flipH $ fibo10 12111
fibo4 = flipH $ fibo10 14111
fibo5 = flipH $ fibo10 14211
fibo6 = flipH $ fibo10 15111
fibo7 = flipH $ fibo10 13124
fibo8 = flipH $ turn 72 $ fibo 500 144 72 13124 -- hor=ver=88
fibo9 = turn 225 $ fibo 12111 4150 18 162
fibo10 m = turn 72 $ dropC 100 $ fibo m 500 144 72

cf1 = overlay [cant3,scale 5 $ fibo3]
cf2 = overlay [cant5,flipV cant5,scale 5 fibo7]
cf3 = overlay [cant5,flipV cant6,scale 5 fibo7]
cf4 = overlay [cant6,flipV cant6,scale 5 fibo7] -- hor=ver=18

rads1 = map (*11) $ [1..9]++reverse [2..8]
rads2 = map (*11) [2,2,3,3,4,4,5,5,4,4,3,3]

poly1 = poly 12111 11 rads1
poly2 = poly 13111 11 rads1
poly3 = poly 14111 11 rads1
poly4 = poly 14211 11 rads1
poly5 = poly 13121 11 rads2
poly6 = combine [poly2,turn (-1.5) poly5]   -- hor=ver=4
poly7 = poly 12111 50 [44]
poly8 = poly 12111 5 [1,44]
poly9 = hueCol 1 cyan $ morph1 44
poly10 = hueCol 1 cyan $ updMod 14211 $ morph1 44
poly11 = updMod 12121 $ morph1 44
poly12 = turn 36 $ scale 0.5 poly8
poly13 = poly 12111 2 [0.1]
poly14 n r = turn 18 $ poly n 5 [r,r/3]
poly15 n r = turn (-18) $ poly n 5 [r,r/3]

morph1 n = morphing 1 n [poly7,poly8]
morph2 n = turn 18 $ morphing 1 n [poly8,poly12,turn 72 poly8]
morph3 n = updMod 23121 $ morph2 n
morph4 n = morphing 1 n [poly7,poly13]
morph5 n = morphing 1 n [turn 45 $ poly 12111 4 [55],tria 12111 33]
morph6 n = morphing 1 n [cant3,scale 0.15 poly1]

cp1 = combine [cant3,scale 0.15 poly1]
cp2 = combine [updMod 12121 $ cant3,scale 0.15 poly1]
cp3 = combine [updMod 12121 $ cant3,scale 0.25 poly6] -- hor=ver=18
cp4 = overlay [cant4,flipV cant4,scale 0.25 poly6]

snow1 m n = snow m 4 33 6 $ tria n 1
snow2 m n = snow m 5 33 6 $ tria n (-1)
snow3 m n = shiftCol 111 $ snow1 m n
snow4 m   = snow m 4 33 6 $ updCol cyan $ elli 1 1
snow8 m n = snow m 5 33 6 $ updCol yellow $ tria n 1
snow9 m n = snow m 4 33 6 $ poly n 6 [1,1/3]
snow10 m n = snow m 4 33 4 $ poly n 4 [1,1/3]
snow11 m n = snow m 4 33 4 $ poly n 8 [1,1/3]
snow12 m n = snow m 4 33 8 $ poly n 4 [1,1/3]
snow13 m n = snow m 4 33 5 $ poly14 n 1
snow14 m n = snow m 4 33 10 $ poly14 n 1
snow15 m n = snow m 4 33 15 $ poly14 n 1
snow16 m n = snow m 4 44 4 $ rect n (0.5,0.5)
snow17 m n = snow m 3 44 8 $ rect n (0.4,0.4)
snow18 m n = snow m 4 44 6 $ rect n (0.5,0.5)
snow19 m n = snow m 6 44 6 $ rect n (0.5,0.5)
snow20 m n = snow m 5 44 5 $ poly14 n 1
snow21 m n = snow m 5 44 4 $ rect n (1,1)
snow22 m n = snow m 4 33 6 $ tria n (-1)
snow23 m n = snow m 4 33 6 $ combine [tria n 1,tria n (-1)]
snow24 m n = snow m 4 33 6 $ circ n 1
snow25 m n = snow m 4 33 6 $ circ n 0.5
snow26 k m n = hueCol k yellow $ snow m 4 33 10 $ poly14 n 1

phyllo1 = phyllo 21111 300          -- hor=ver=14
phyllo2 = phyllo 41111 300  
phyllo3 = updCol yellow $ phyllo 21111 600
phyllo4 = updCol yellow $ phyllo 21113 600
phyllo5 = updCol yellow $ phyllo 21114 600
phyllo6 = updCol yellow $ phyllo 21124 600
phyllo7 = updCol yellow $ phyllo 21134 600

sp1 = combine [snow1 4 12111,scale 0.7 phyllo1] -- hor=ver=12
sp2 = shiftCol 500 sp1          
sp3 = shiftCol 1000 sp1         

circ1 = circ 13111 33
circ2 = circ 15111 33
rect1 = rect 15211 (45,5) 
rect2 = rect 13211 (5,45)

rain1 = rainbow 1 33 circ1
rain2 = rainbow 1 33 circ2
rain3 = rainbow 1 33 rect1
rain4 = rainbow 1 33 rect2
rain5 m n = rainbow m n $ tria 13111 33
rain6 m n = rainbow m n $ tria 13113 33

morph8 n    = morphing 1 n $ map (rainbow 1 33) [rect1,rect2]
morph9 m n  = morphing m n $ map (rainbow 1 33. updCol yellow . rect 15211) 
                             [(5,45),(45,5)]
morph10 m n = morphing m n $ map (rainbow 1 33 . updCol cyan . rect 15214) 
                     [(50,30),(0,30),(50,0)]
morph11 m n = morphing m n $ map (rainbow 1 33 . updCol cyan . rect 15211) 
                     [(50,25),(1,22),(50,5)]
morph12 m n = morphing m n $ map ($ poly 13111 12 [2,12]) [id,shift (50,0)]          
morph13 m n = morphing m n [circ1,scale 0 circ1] -- rainbow m n circ1
morph14 m n = morphing m n [circ2,scale 0 circ2] -- rainbow m n circ2
morph15 m n = morphing m n [circ1,shift (15,15) $ scale 0 circ1]    
morph16 m n = morphing m n [circ2,shift (15,15) $ scale 0 circ2]
morph17 m n = morphing m n $ map ($ snow1 6 15111) [id,shift (10,0) . scale 0]
morph18 m n = morphing m n $ map ($ snow1 6 15111) [id,scale 0]
                                 -- rainbow m n $ snow1 6 15111
morph19 m n = morphing m n $ map ($ snow1 6 15111) [hscale 0.5,vscale 0.5]
morph20 m n = morphing m n $ map ($ snow1 6 15111) [hscale 0,id,vscale 0]

curves1 m n = morphs m n $ map ($ snow1 6 15111) [id,scale 0]

path1 = kpath 6 1 1 12111
path2 = kpath 8 1 1 14211
path3 = kpath 8 8 8 14211
path4 = overlay [path1,flipH path1,path2,flipH path2]
path5 = kpath 20 1 1 12111
path6 = kpath 40 1 1 14211
path7 = kpath 40 40 40 12113

{- kpath 6 1 1 -->
   Just [(1,1), (2,3), (1,5), (3,6), (5,5), (6,3), (5,1), (3,2), (1,3), (2,1),
         (4,2), (6,1), (5,3), (6,5), (4,6), (3,4), (2,6), (1,4), (2,2), (4,1),
     (6,2), (5,4), (6,6), (4,5), (3,3), (2,5), (4,4), (5,6), (6,4), (5,2),
     (3,1), (1,2), (2,4), (1,6), (3,5), (4,3)] -}

tour1 = ktour 8 1 1 12211       -- hor=ver=44
tour2 = ktour 8 8 8 14211
tour3 = overlay [tour1,flipH tour1,tour2,flipH tour2]
tour4 = ktour 16 8 8 12111
tour5 = ktour 18 9 9 12111      -- hor=ver=22
tour6 = ktour 18 9 9 12113
tour7 = ktour 18 9 9 12114
tour8 = toCenter $ ktour 18 9 9 13111
tour9 = hueCol 1 red $ overlay [p,flipH p,q,flipH q]
        where p = ktour 8 1 1 14211; q = ktour 8 8 8 14211

{- ktour 8 1 1 -->
   Just [(1,1), (2,3), (3,1), (1,2), (2,4), (1,6), (2,8), (4,7), (6,8), (8,7),
         (7,5), (8,3), (7,1), (5,2), (7,3), (8,1), (6,2), (4,1), (2,2), (1,4), 
     (2,6), (1,8), (3,7), (5,8), (7,7), (8,5), (6,6), (7,8), (8,6), (7,4), 
     (8,2), (6,1), (4,2), (2,1), (3,3), (5,4), (3,5), (4,3), (5,1), (6,3),
     (8,4), (7,2), (6,4), (5,6), (4,8), (2,7), (1,5), (3,6), (1,7), (3,8), 
     (5,7), (4,5), (5,3), (6,5), (4,6), (6,7), (8,8), (7,6), (5,5), (3,4), 
     (1,3), (2,5), (4,4), (3,2), (1,1)] -}