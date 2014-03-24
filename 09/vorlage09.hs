module Blatt09 where

data BintreeL a 
  = Leaf 
  | Bin a (BintreeL a) (BintreeL a)

tree :: BintreeL Int
tree = Bin 5 (Bin 6 t1 t2) (Bin 4 t3 t4) where
  t1 = Bin 8 Leaf Leaf
  t2 = Bin 1 (Bin 2 Leaf Leaf) Leaf
  t3 = Bin 11 Leaf (Bin 72 Leaf (Bin 3 Leaf Leaf))
  t4 = Leaf

  
type Node = [Int]

type TreeNode a = (BintreeL a,Node) 
data Context a = Top 
               | L a (Context a) (BintreeL a) 
               | R a (BintreeL a) (Context a)

type TreeZipper a = (Context a,BintreeL a)

treeToZipper :: TreeNode a -> TreeZipper a
treeToZipper (t,node) = loop Top t node where 

  loop c (Bin a t u) (0:node) = loop (L a c u) t node
  loop c (Bin a t u) (1:node) = loop (R a t c) u node
  loop c t _ = (c,t)

