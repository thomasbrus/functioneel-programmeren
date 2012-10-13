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
insert (Leaf) a = Node Black a Leaf Leaf
insert (Node c n r1 r2) a
  | a > n
  = Node c n r1 (insert r2 a)
  | otherwise
  = Node c n (insert r1 a) r2

exampleTree :: RBTree
exampleTree =
  (Node Black 13
    (Node Red 8
      (Node Red 1
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
rootToBlack (Node Red n r1@(Node Red _ _ _) r2) = Node Black n r1 r2
rootToBlack (Node Red n r1 r2@(Node Red _ _ _)) = Node Black n r1 r2
rootToBlack r = r

-- 3. In het tweede geval hebben de twee subbomen A en B van een
-- zwarte node N allebei een rode root. Bovendien heeft een van deze
-- beide subbomen zelf een subboom met een rode root C. Schrijf een
-- functie colourFlip die de zwarte root N rood maakt, en de rode
-- roots A en B van de twee subbomen zwart. Node C blijft dus rood.

--     N (Black)                N (Red)
--      /   \                  /     \
--     /     \                /       \
--    A (Red) B (Red)        A (Black) B (Black)
--   /         \            /           \
--  *           C (Red)    *             C (Red)
--

colorFlip :: RBTree -> RBTree
colorFlip (Node Black n r1@(Node Red na r1a@(Node Red _ _ _) r1b) r2)
  = Node Red n (Node Black na r1a r1b) r2
colorFlip (Node Black n r1@(Node Red na r1a r1b@(Node Red _ _ _)) r2)
  = Node Red n (Node Black na r1a r1b) r2
colorFlip (Node Black n r1 r2@(Node Red na r2a@(Node Red _ _ _) r2b))
  = Node Red n r1 (Node Black na r2a r2b)
colorFlip (Node Black n r1 r2@(Node Red na r2a r2b@(Node Red _ _ _)))
  = Node Red n r1 (Node Black na r2a r2b)
colorFlip r = r

