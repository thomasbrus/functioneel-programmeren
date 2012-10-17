module RBTree where

import Data.Char
import FPPrac.Prelude

data Color = Red | Black | Gray deriving (Show, Eq)
data RBTree = Leaf Color | Node Color Number RBTree RBTree deriving (Show, Eq)

-- 1. Schrijf een functie insert die het eigenlijke inserten voor
-- zijn rekening neemt zonder te letten op de rood-zwart eigenschap.
-- Een nieuwe waarde moet als rode node ge-insert worden op de
-- plaats van een blad, uiteraard met inachtneming van de ordening
-- in de boom.

insert :: RBTree -> Number -> RBTree
insert (Leaf _) a = Node Red a (Leaf Black) (Leaf Black)
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
        (Leaf Black)
        (Node Red 6
          (Leaf Black)
          (Leaf Black)
        )
      )
      (Node Black 11
        (Leaf Black)
        (Leaf Black)
      )
    )
    (Node Red 17
      (Node Black 15
        (Leaf Black)
        (Leaf Black)
      )
      (Node Black 25
        (Node Red 22
          (Leaf Black)
          (Leaf Black)
        )
        (Node Red 27
          (Leaf Black)
          (Leaf Black)
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

colorFlip :: RBTree -> RBTree
colorFlip (Node Black v1 (Node Red v2 t4@(Node Red _ _ _) t5) (Node Red v3 t6 t7))
  = Node Red v1 (Node Black v2 t4 t5) (Node Black v3 t6 t7)
colorFlip (Node Black v1 (Node Red v2 t4 t5@(Node Red _ _ _)) (Node Red v3 t6 t7))
  = Node Red v1 (Node Black v2 t4 t5) (Node Black v3 t6 t7)
colorFlip (Node Black v1 (Node Red v2 t4 t5) (Node Red v3 t6@(Node Red _ _ _) t7))
  = Node Red v1 (Node Black v2 t4 t5) (Node Black v3 t6 t7)
colorFlip (Node Black v1 (Node Red v2 t4 t5) (Node Red v3 t6 t7@(Node Red _ _ _)))
  = Node Red v1 (Node Black v2 t4 t5) (Node Black v3 t6 t7)
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
rebalance (Node Black v1 t2@(Node Red v2 t4@(Node Red _ _ _) t5) t3)
  = Node Black v2 t4 (Node Red v1 t5 t3)
rebalance (Node Black v1 t2@(Node Red v2 t4 t5@(Node Red v5 t10 t11)) t3)
  = Node Black v5 (Node Red v2 t4 t10) (Node Red v1 t11 t3)
rebalance (Node Black v1 t2 t3@(Node Red v3 t6 t7@(Node Red _ _ _)))
  = Node Black v3 (Node Red v1 t6 t2) t7
rebalance (Node Black v1 t2 t3@(Node Red v3 t6@(Node Red v6 t12 t13) t7))
  = Node Black v6 (Node Red v3 t7 t13) (Node Red v1 t12 t2)
rebalance t = t

-- 5. Schrijf een functie balancedInsert die eerst een element toevoegt
-- aan een boom, en vervolgens de rood-zwart eigenschap herstelt.
rebalanceTree :: RBTree -> RBTree
rebalanceTree (Leaf c) = Leaf c
rebalanceTree t = let (Node c n t1 t2) = rebalance t
                  in Node c n (rebalanceTree t1) (rebalanceTree t2)

balancedInsert :: RBTree -> Number -> RBTree
balancedInsert t a = rebalanceTree (insert t a)


-- 1. Schrijf een functie leftmostValue die het element in de meest linker in-
-- terne node van een (sub)boom oplevert.

--leftmostValue :: RBTree -> Number
--leftmostValue (Node _ n Leaf _) = n
--leftmostValue (Node _ _ t1 _) = leftmostValue t1

-- 2. Schrijf een functie removeLeftmostNode die de meest linker interne
-- node van een (sub)boom verwijdert en op de vrijgekomen plaats de rechter
-- subboom van die node weer neerzet (merk op dat daardoor geen verdere
-- informatie verloren gaat). Ga de mogelijke gevallen na, en bedenk dat
-- bij RB-bomen die meest linker node altijd op de onderste regel (in de
-- RB-weergave) staat.

-- Bij het verwijderen kan de RB-eigenschap vaak direct gehandhaafd blij-
-- ven. Alleen in het geval dat de te verwijderen leftmost node een zwarte
-- “eindnode” is (dus zelf alleen maar bladeren als subbomen heeft) kan dat
-- niet en moet hij vervangen worden door een grijs blad.

--removeLeftmostNode :: RBTree -> RBTree
--removeLeftmostNode (Node _ n Leaf Leaf) = t2
--removeLeftmostNode (Node _ n Leaf t2) = t2
--removeLeftmostNode (Node c n t1 t2) = Node c n (removeLeftmostNode t1) t2


