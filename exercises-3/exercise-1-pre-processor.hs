module PreProcessor where

import RoseTree
import FPPrac

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

preProcess1a :: Tree1a -> RoseTree
preProcess1a (Leaf1a n)       = RoseNode (show n) []
preProcess1a (Node1a n t1 t2) = RoseNode (show n) [(preProcess1a t1), (preProcess1a t2)]

exampleTree1a = Node1a 1 (Leaf1a 2) (Leaf1a 3) 

--main = showTree (preProcess1a exampleTree1a)

data Tree1b = Leaf1b (Number, Number) | Node1b (Number, Number) Tree1b Tree1b

preProcess1b :: Tree1b -> RoseTree
preProcess1b (Leaf1b nt)        = RoseNode (show (nt)) []
preProcess1b (Node1b nt t1 t2)  = RoseNode (show (nt)) [(preProcess1b t1), (preProcess1b t2)]

exampleTree1b = Node1b (1, 2) (Leaf1b (2, 3)) (Leaf1b (3, 4)) 

--main = showTree (preProcess1b exampleTree1b)

data Tree1c = Leaf1c | Node1c Number Tree1c Tree1c

preProcess1c :: Tree1c -> RoseTree
preProcess1c (Leaf1c)          = RoseNode "" []
preProcess1c (Node1c n t1 t2)  = RoseNode (show n) [(preProcess1c t1), (preProcess1c t2)]

exampleTree1c = Node1c 1 Leaf1c Leaf1c

--main = showTree (preProcess1c exampleTree1c)

data Tree1d = Leaf1d (Number, Number) | Node1d [Tree1d]

preProcess1d :: Tree1d -> RoseTree
preProcess1d (Leaf1d nt)  = RoseNode (show (nt)) []
preProcess1d (Node1d ts)  = RoseNode "" (map preProcess1d ts)

exampleTree1d = Node1d [(Leaf1d (1, 2)), (Leaf1d (3, 4)), (Leaf1d (5, 6))]

--main = showTree (preProcess1d exampleTree1d)

