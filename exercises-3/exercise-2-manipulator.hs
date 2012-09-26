import RoseTree
import FPPrac
import PreProcessor

increaseTreeValues :: Number -> Tree1a -> Tree1a
increaseTreeValues a (Leaf1a n) = Leaf1a (n + a)
increaseTreeValues  a (Node1a n t1 t2) = Node1a (n + a) (increaseTreeValues a t1) (increaseTreeValues a t2)

--main = showTree (preProcess1a (increaseTreeValues 3 exampleTree1a))

quadrateTreeValues :: Tree1a -> Tree1a
quadrateTreeValues (Leaf1a n) = Leaf1a (n * n)
quadrateTreeValues (Node1a n t1 t2) = Node1a (n * n) (quadrateTreeValues t1) (quadrateTreeValues t2)

--main = showTree (preProcess1a (quadrateTreeValues exampleTree1a))

mapTree :: (Number -> Number) -> Tree1a -> Tree1a
mapTree f (Leaf1a n) = Leaf1a (f n)
mapTree f (Node1a n t1 t2) = Node1a (f n) (mapTree f t1) (mapTree f t2)

--main = showTree (preProcess1a (mapTree (\x -> x * 3) exampleTree1a))

sumNodes :: Tree1b -> Tree1a
sumNodes (Leaf1b (n1, n2)) = Leaf1a (n1 + n2) 
sumNodes (Node1b (n1, n2) t1 t2) = Node1a (n1 + n2) (sumNodes t1) (sumNodes t2)

--main = showTree (preProcess1a (sumNodes exampleTree1b))

mapTree' :: ((Number, Number) -> Number) -> Tree1b -> Tree1a
mapTree' f (Leaf1b nt) = Leaf1a (f nt)
mapTree' f (Node1b nt t1 t2) = Node1a (f nt) (mapTree' f t1) (mapTree' f t2)

--main = showTree (preProcess1a (mapTree' (\(x, y) -> x + y) exampleTree1b))
--main = showTree (preProcess1a (mapTree' (\(x, y) -> x - y) exampleTree1b))
--main = showTree (preProcess1a (mapTree' (\(x, y) -> x * y) exampleTree1b))

