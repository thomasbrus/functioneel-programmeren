module RBTree where

import Data.Char
import FPPrac.Prelude

data Color = Red | Black deriving (Show, Eq)
data RBTree = Leaf | Node Color Number RBTree RBTree deriving (Show, Eq)

-- 1. Schrijf een functie insert die het eigenlijke inserten voor
-- zijn rekening neemt zonder te letten op de rood-zwart eigenschap.
-- Een nieuwe waarde moet als rode node ge-insert worden op de
-- plaats van een blad, uiteraard met inachtneming van de ordening
-- in de boom.

insert :: RBTree -> Number -> RBTree
insert (Leaf) a = Node Red a Leaf Leaf
insert (Node c n t1 t2) a
  | a > n
  = Node c n t1 (insert t2 a)
  | otherwise
  = Node c n (insert t1 a) t2

exampleTree :: RBTree
exampleTree =
  (Node Black 13
    (Node Red 8
      (Node Black 1
        Leaf
        (Node Red 6
          Leaf
          Leaf
        )
      )
      (Node Black 11
        Leaf
        Leaf
      )
    )
    (Node Red 17
      (Node Black 15
        Leaf
        Leaf
      )
      (Node Black 25
        (Node Red 22
          Leaf
          Leaf
        )
        (Node Red 27
          Leaf
          Leaf
        )
      )
    )
  )

-- Door het inserten kunnen er twee rode nodes direct na elkaar
-- ontstaan zodat niet meer aan de rood-zwart eigenschap is voldaan.
-- Hieronder worden de functies besproken die dat voor de
-- verschillende mogelijke situaties herstellen. Tijdens het
-- herstelproces kunnen de twee opeenvolgende rode nodes naar
-- boven door de boom bewegen, maar er zal nooit meer dan een
-- tweetal opeenvolgende nodes rood zijn.

-- 2. Het simpelste geval is dat de bovenste van de twee rode nodes de
-- root van de hele boom is. Schrijf een functie rootToBlack1 die de
-- root simpelweg weer zwart maakt.

rootToBlack :: RBTree -> RBTree
rootToBlack (Node Red n t1@(Node Red _ _ _) t2) = Node Black n t1 t2
rootToBlack (Node Red n t1 t2@(Node Red _ _ _)) = Node Black n t1 t2
rootToBlack t = t

-- 3. In het tweede geval hebben de twee subbomen A en B van een
-- zwarte node N allebei een rode root. Bovendien heeft een van deze
-- beide subbomen zelf een subboom met een rode root C. Schrijf een
-- functie colourFlip die de zwarte root N rood maakt, en de rode
-- roots A en B van de twee subbomen zwart. Node C blijft dus rood.

-- Merk op dat er nu opnieuw een probleem kan ontstaan omdat de parent
-- van N ook rood kan zijn.

--     N (Black)                N (Red)
--      /   \                  /     \
--     /     \                /       \
--    A (Red) B (Red)        A (Black) B (Black)
--   /         \            /           \
--  *           C (Red)    *             C (Red)
--

colorFlip :: RBTree -> RBTree
colorFlip (Node Black n t1@(Node Red na t1a@(Node Red _ _ _) t1b) t2)
  = Node Red n (Node Black na t1a t1b) t2
colorFlip (Node Black n t1@(Node Red na t1a t1b@(Node Red _ _ _)) t2)
  = Node Red n (Node Black na t1a t1b) t2
colorFlip (Node Black n t1 t2@(Node Red na t2a@(Node Red _ _ _) t2b))
  = Node Red n t1 (Node Black na t2a t2b)
colorFlip (Node Black n t1 t2@(Node Red na t2a t2b@(Node Red _ _ _)))
  = Node Red n t1 (Node Black na t2a t2b)
colorFlip t = t

-- 4. Het derde geval is dat een zwarte root slechts één rood kind heeft,
-- maar dat dit rode kind zelf ook weer een rood kind heeft. Schrijf
-- een functie rebalance die de vier mogelijke situaties2 waarin dit
-- optreedt zodanig bewerkt dat weer aan de RB-eigenschap is voldaan.
-- Formuleer daartoe deze gevallen als patterns van uw type.

-- (Zie voorbeeld (b) in rbtrees.pdf)

-- t1   = b
-- t1a  = c
-- n    = a
-- n1   = b

rebalance :: RBTree -> RBTree
rebalance t@(Node Black n t1@(Node Red n1 t1a@(Node Red _ _ _) t1b) t2@(Node Black _ _ _))
  = Node Black n1 t1a t
rebalance t@(Node Black n t1@(Node Red _ t1a t1b@(Node Red _ _ _)) t2@(Node Black n2 _ _))
  = Node Black n2 t1 t
rebalance t@(Node Black n t1@(Node Black n1 _ _) t2@(Node Red _ t2a@(Node Red _ _ _) t2b))
  = Node Black n1 t t2
rebalance t@(Node Black n t1@(Node Black _ _ _) t2@(Node Red n2 t2a t2b@(Node Red _ _ _)))
  = Node Black n2 t t2b
rebalance t = t

-- 5. Schrijf een functie balancedInsert die eerst een element toevoegt
-- aan een boom, en vervolgens de rood-zwart eigenschap herstelt.
rebalanceTree :: RBTree -> RBTree
rebalanceTree (Leaf) = Leaf
rebalanceTree t = let (Node c n t1 t2) = rebalance t
                  in Node c n (rebalance t1) (rebalance t2)

balancedInsert :: RBTree -> Number -> RBTree
balancedInsert t a = rebalanceTree (insert t a)

--Node Black 13
--  (Node Red 8
--    (Node Black 1
--      Leaf
--      (Node Red 6
--        (Node Red 4
--          Leaf
--          Leaf
--        ) 
--        Leaf
--      )
--    )
--    (Node Black 11
--      Leaf
--      Leaf
--    )
--  )
--  (Node Red 17
--    (Node Black 15
--      Leaf
--      Leaf)
--    (Node Black 25
--      (Node Red 22
--        Leaf
--        Leaf
--      )
--      (Node Red 27
--        Leaf
--        Leaf
--      )
--    )
--  )

--Node Black 13
--  (Node Red 8
--    (Node Black 1
--      Leaf
--      (Node Red 6
--        Leaf
--        Leaf
--      )
--    )
--    (Node Black 11
--      Leaf
--      Leaf
--    )
--  )
--  (Node Red 17
--    (Node Black 15
--      Leaf
--      Leaf
--    )
--    (Node Black 25
--      (Node Red 22
--        Leaf
--        Leaf
--      )
--      (Node Red 27
--        Leaf
--        Leaf
--      )
--    )
--  )

leftmostValue :: RBTree -> Number
leftmostValue (Node _ n Leaf _) = n
leftmostValue (Node _ _ t1 _) = leftmostValue t1

