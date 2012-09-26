import RoseTree
import FPPrac
import PreProcessor

mirrorTree :: Tree1a -> Tree1a
mirrorTree (Leaf1a n) = Leaf1a n
mirrorTree (Node1a n t1 t2) = Node1a n (mirrorTree t2) (mirrorTree t1)

--main = showTreeList
--  [ (preProcess1a exampleTree1a)
--  , (preProcess1a (mirrorTree exampleTree1a))
--  , (preProcess1a (mirrorTree (mirrorTree exampleTree1a)))
--  ]

mirrorTree' :: Tree1d -> Tree1d
mirrorTree' (Leaf1d (n1, n2)) = Leaf1d (n2, n1)
mirrorTree' (Node1d ts)       = Node1d (map mirrorTree' (reverse ts))

--main = showTreeList
--  [ (preProcess1d exampleTree1d)
--  , (preProcess1d (mirrorTree' exampleTree1d))
--  , (preProcess1d (mirrorTree' (mirrorTree' exampleTree1d)))
--  ]

