module SortedTree where

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

insertInTree :: Number -> Tree1c -> Tree1c
insertInTree x (Leaf1c)         = Node1c x Leaf1c Leaf1c
insertInTree x (Node1c n t1 t2) | x > n     = Node1c n t1 (insertInTree x t2)
                                | otherwise = Node1c n (insertInTree x t1) t2

--main = showTree (preProcess1c (insertInTree 7 extendedExampleTree))

createTree :: [Number] -> Tree1c
createTree [x]    = insertInTree x Leaf1c
createTree (x:xs) = insertInTree x (createTree xs)

createTree' :: [Number] -> Tree1c
createTree' = foldr insertInTree Leaf1c

--main = showTreeList
--  [ (preProcess1c (createTree [10, 4, 5, 2, 6, 9]))
--  , (preProcess1c (createTree' [10, 4, 5, 2, 6, 9])) ]

createList :: Tree1c -> [Number]
createList (Leaf1c)         = []
createList (Node1c n t1 t2) = (createList t1) ++ [n] ++ (createList t2)

--main = putStrLn (show (createList extendedExampleTree))

sortList :: [Number] -> [Number]
sortList xs = createList (createTree xs)

--main = putStrLn (show (sortList [4, 1, 7, 9, 0]))

sortTree :: Tree1c -> Tree1c
sortTree t = createTree (createList t)

--main = showTree (preProcess1c (sortTree extendedExampleTree))
