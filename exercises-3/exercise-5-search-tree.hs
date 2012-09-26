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

searchTree :: Number -> Tree1c -> Tree1c
searchTree x Leaf1c           = error "Number not found in tree"
searchTree x (Node1c n t1 t2) | x == sqrt(n)    = Node1c n t1 t2
                              | x > n           = searchTree x t2
                              | otherwise       = searchTree x t1

--main = showTree (preProcess1c (searchTree 1 extendedExampleTree))
