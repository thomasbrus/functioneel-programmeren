-- Opgave 8. Een binaire boom is gebalanceerd als alle takken zoveel mo-
-- gelijk even lang zijn (dus hoogstens een stap in lengte verschillen).
-- Gebruik bomen van type Tree1c.

-- $ ghci exercises-3/exercise-8-balanced-tree.hs exercises-3/exercise-1-pre-processor.hs exercises-3/RoseTree.hs exercises-3/exercise-4-sorted-tree.hs exercises-2/exercise-5-increasing-lists.hs

import RoseTree
import FPPrac
import PreProcessor
import SortedTree
import ListUtils
import Data.List (sort)

-- a. Schrijf een functie die test of een boom gebalanceerd is.

myTree = Node1c 4
  (Node1c 1
    (Node1c 0 Leaf1c Leaf1c)
    (Node1c 2 Leaf1c Leaf1c)
  )
  (Node1c 5
    (Node1c 3 
      (Node1c 11
        (Node1c 4 Leaf1c Leaf1c)
        (Node1c 1 Leaf1c Leaf1c)
      )
      (Node1c 12 Leaf1c Leaf1c)
    )
    (Node1c 8 Leaf1c Leaf1c)
  )

myTree' = Node1c 4
  (Node1c 1
    (Node1c 0 Leaf1c Leaf1c)
    (Node1c 2 Leaf1c Leaf1c)
  )
  (Node1c 3 
    (Node1c 11 Leaf1c Leaf1c)
    (Node1c 12 Leaf1c Leaf1c)
  )

isBalanced :: Tree1c -> Bool
isBalanced t  = let lengths = branchLengths t in
                (maximum lengths) - (minimum lengths) <= 1

branchLengths :: Tree1c -> [Int]
branchLengths (Leaf1c) = []
branchLengths (Node1c _ Leaf1c Leaf1c) = [1]
branchLengths (Node1c _ t1 t2)  = map (+ 1) (branchLengths t1) ++
                                  map (+ 1) (branchLengths t2)

--main = putStrLn . show $ isBalanced myTree

-- b. Schrijf een functie die van een boom een gebalanceerde boom maakt
-- (hint: gebruik type Tree1c, en werk via de tussenstap van lijsten.
-- Ga na waarom deze benadering voor bomen van type Tree1c makkelijker
-- is dan voor bomen van type Tree1a).
-- Ga met uw testfunctie uit onderdeel a na of uw functie uit onderdeel
-- b goed werkt.

balanceTree :: Tree1c -> Tree1c
balanceTree Leaf1c = Leaf1c
balanceTree t = Node1c m (balanceTree t1) (balanceTree t2)
  where
    xs = sort $ createList t
    (l1, l2) = break (> (average xs)) xs
    (l1', m)  | length l1 == 1 = ([], last l1)
              | otherwise = (init l1, last l1)
    t1  | null l1'  = Leaf1c
        | otherwise = createTree' l1'
    t2  | null l2   = Leaf1c
        | otherwise = createTree' l2

--*Main> isBalanced $ balanceTree myTree
--True
--*Main> isBalanced myTree
--False

