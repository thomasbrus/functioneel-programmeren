module RBTree where

import Data.Char
import FPPrac.Prelude

data Color = Red | Black deriving (Show)
data RBTree = Leaf | Node Color Number RBTree RBTree deriving (Show)

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