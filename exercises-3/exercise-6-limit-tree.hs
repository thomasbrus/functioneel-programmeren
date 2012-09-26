import RoseTree
import FPPrac
import PreProcessor

extendedExampleTree = Node1c 4
  (Node1c 1
    (Node1c 0 Leaf1c Leaf1c)
    (Node1c 2 Leaf1c Leaf1c)
  )
  (Node1c 5
    (Node1c 3 Leaf1c Leaf1c)
    (Node1c 8 Leaf1c Leaf1c)
  )

limitTree :: Number -> Tree1c -> Tree1c
limitTree _ (Leaf1c) = Leaf1c
limitTree 1 (Node1c n t1 t2) = Node1c n Leaf1c Leaf1c
limitTree m (Node1c n t1 t2) = Node1c n (limitTree (m - 1) t1) (limitTree (m - 1) t2)

--main = showTree (preProcess1c (limitTree 2 extendedExampleTree))
