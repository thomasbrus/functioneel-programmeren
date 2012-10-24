module RBTree where

import Data.Char
import FPPrac.Prelude
import qualified FPPrac.Graphics as G
import RBGraphics

data Color = Red | Black | Gray deriving (Show, Eq)
data RBTree = Leaf Color | Node Color Number RBTree RBTree deriving (Show, Eq)

ppTree (Leaf c) = RBnode (ppC c) [] []
ppTree (Node c n t1 t2) = RBnode (ppC c) (show n) [ppTree t1,ppTree t2]

ppC Red   = G.red
ppC Black = G.black
ppC Gray  = G.dark G.white

isRed :: RBTree -> Bool
isRed (Node Red _ _ _) = True
isRed (Leaf Red) = True
isRed _ = False

isGray :: RBTree -> Bool
isGray (Node Gray _ _ _) = True
isGray (Leaf Gray) = True
isGray _ = False

isBlack :: RBTree -> Bool
isBlack (Node Black _ _ _) = True
isBlack (Leaf Black) = True
isBlack _ = False

makeBlack :: RBTree -> RBTree
makeBlack (Node _ n t1 t2) = Node Black n t1 t2
makeBlack (Leaf _) = Leaf Black

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
rootToBlack t@(Node Red n t1 t2)
  | isRed t1 || isRed t2 = Node Black n t1 t2
  | otherwise            = t
rootToBlack t = t

-- 3. In het tweede geval hebben de twee subbomen A en B van een
-- zwarte node N allebei een rode root. Bovendien heeft een van deze
-- beide subbomen zelf een subboom met een rode root C. Schrijf een
-- functie colourFlip die de zwarte root N rood maakt, en de rode
-- roots A en B van de twee subbomen zwart. Node C blijft dus rood.

-- Merk op dat er nu opnieuw een probleem kan ontstaan omdat de parent
-- van N ook rood kan zijn.

colorFlip :: RBTree -> RBTree

colorFlip t@(Node Black v1 (Node Red v2 t4 t5) (Node Red v3 t6 t7))
  | any isRed [t4, t5, t6, t7]
  = Node Red v1 (Node Black v2 t4 t5) (Node Black v3 t6 t7)
  | otherwise
  = t
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
rebalanceTree t = let (Node c n t1 t2) = (rebalance . colorFlip) t
                  in Node c n (rebalanceTree t1) (rebalanceTree t2)

balancedInsert :: RBTree -> Number -> RBTree
balancedInsert t a = rootToBlack $ rebalanceTree (insert t a)

-- *RBTree> insert exampleTree 10

-- Node Black 13
--  (Node Red 8
--    (Node Black 1
--      (Leaf Black)
--      (Node Red 6
--        (Leaf Black)
--        (Leaf Black)
--      )
--    )
--    (Node Black 11
--      (Node Red 10
--        (Leaf Black)
--        (Leaf Black)
--      )
--      (Leaf Black)
--    )
--  )
--  (Node Red 17
--    (Node Black 15
--      (Leaf Black)
--      (Leaf Black)
--    )
--    (Node Black 25
--      (Node Red 22
--        (Leaf Black)
--        (Leaf Black)
--      )
--      (Node Red 27
--        (Leaf Black)
--        (Leaf Black)
--      )
--    )
--  )

-- *RBTree> balancedInsert exampleTree 10

-- Node Black 13
--  (Node Red 8
--    (Node Black 1
--      (Leaf Black)
--      (Node Red 6
--        (Leaf Black)
--        (Leaf Black)
--      )
--    )
--    (Node Black 11
--      (Node Red 10
--        (Leaf Black)
--        (Leaf Black)
--      )
--      (Leaf Black)
--    )
--  )
--  (Node Red 17
--    (Node Black 15
--      (Leaf Black)
--      (Leaf Black)
--    )
--    (Node Black 25
--      (Node Red 22
--        (Leaf Black)
--        (Leaf Black)
--      )
--      (Node Red 27
--        (Leaf Black)
--        (Leaf Black)
--      )
--    )
--  )

-- 1. Schrijf een functie leftmostValue die het element in de meest linker in-
-- terne node van een (sub)boom oplevert.

leftmostValue :: RBTree -> Number
leftmostValue (Node _ n (Leaf _) _) = n
leftmostValue (Node _ _ t1 _) = leftmostValue t1

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

removeLeftmostNode :: RBTree -> RBTree
removeLeftmostNode (Node _ _ (Leaf _) (Leaf _)) = Leaf Gray
removeLeftmostNode (Node _ _ (Leaf _) t2@(Node _ _ _ _)) = t2
removeLeftmostNode (Node c n t1 t2) = Node c n (removeLeftmostNode t1) t2

-- 3. Schrijf een functie greyColourFlip die locaal subbomen met een grijze
-- node omzet. Formuleer daartoe de vijf3 in het boek genoemde gevallen als
-- patterns van uw boomtype.
-- Hou bij de clauses in uw functiedefinitie de volgorde van de patterns
-- in het boek aan, dan kunnen ze eventueel wat compacter worden geformuleerd.
-- Vergeet niet de vijf symmetrische gevallen waarbij de grijze node in de
-- rechter subboom zit.

grayColorFlip :: RBTree -> RBTree
-- Eerste en vierde geval
grayColorFlip p@(Node Black pv g s@(Node Black sv l r))
  | isGray g && isBlack l && isBlack r
  = Node Gray pv (makeBlack g) (Node Red sv l r)
  | isGray g && isBlack l && isRed r
  = Node Black sv (Node Black pv (makeBlack g) l) (makeBlack r)
  | otherwise = p
-- Gespiegelde versie
grayColorFlip p@(Node Black pv s@(Node Black sv r l) g)
  | isGray g && isBlack l && isBlack r
  = Node Gray pv (Node Red sv r l) (makeBlack g)
  | isGray g && isBlack l && isRed r
  = Node Black sv (makeBlack r) (Node Black pv l (makeBlack g))
  | otherwise = p

grayColorFlip p@(Node pc pv g s@(Node Black sv l@(Node Red lv a b) r))
  | isGray g && isBlack a && isBlack b
  -- Welke kleur moet deze node zijn?
  = Node pc lv (Node Black pv (makeBlack g) a) (Node Black sv b r)
  | otherwise = p
-- Gespiegelde versie
grayColorFlip p@(Node pc pv s@(Node Black sv r l@(Node Red lv b a)) g)
  | isGray g && isBlack a && isBlack b
  -- Welke kleur moet deze node zijn?
  = Node pc lv (Node Black sv r b) (Node Black pv a (makeBlack g))
  | otherwise = p

grayColorFlip p@(Node Red pv g s@(Node Black sv l r))
  | isGray g && isBlack l
  = Node Black sv (Node Red pv (makeBlack g) l) r
  | otherwise = p
-- Gespiegelde versie
grayColorFlip p@(Node Red pv s@(Node Black sv r l) g)
  | isGray g && isBlack l
  = Node Black sv r (Node Red pv l (makeBlack g))
  | otherwise = p

-- Vierde geval
--grayColorFlip p@(Node Black pv g s@(Node Black sv l r))
--  | isGray g && isBlack l && isRed r
--  = Node Black sv (Node Black pv (makeBlack g) l) (makeBlack r)
--  | otherwise = p
--grayColorFlip p@(Node Black pv s@(Node Black sv l r) g)
--  | isGray g && isBlack l && isRed r
--  = Node Black sv (Node Black pv (makeBlack g) l) (makeBlack r)
--  | otherwise = p

grayColorFlip p@(Node Black pv g s@(Node Red sv l r))
  | isGray g && isBlack l && isBlack r
  = Node Black sv (Node Red pv g l) r
  | otherwise = p
-- Gespiegelde versie
grayColorFlip p@(Node Black pv s@(Node Red sv r l) g)
  | isGray g && isBlack l && isBlack r
  = Node Black sv r (Node Red pv l g) 
  | otherwise = p

grayColorFlip t = t

-- 4. Schrijf een functie greyRebalance die een boom met een grijze node er in
-- recursief doorloopt en de RB-eigenschap herstelt middels het aanroepen van de
-- functie greyColourFlip.

grayBalance :: RBTree -> RBTree
grayBalance (Leaf c) = Leaf c
grayBalance t = let (Node c n  t1 t2) = grayColorFlip t
                in (Node c n (grayBalance t1) (grayBalance t2))

-- 5. Schrijf een functie delete die een gegeven waarde uit een gegeven RB-
-- boom verwijdert en de RB-eigenschap herstelt. Daartoe moet eerst de node
-- met de te verwijderen waarde opgezocht worden, vervolgens moet de waarde
-- van die node vervangen worden door de waarde van de leftmost interne node
-- van zijn rechter subboom, en deze leftmost node moet ver- wijderd worden
-- (merk op dat deze node de eerstvolgende waarde in de odre binnen de boom bevat).
-- Tenslotte moet de RB-eigenschap worden hersteld.

-- Opmerking: als de node met de te verwijderen waarde geen rechter sub-
-- boom meer heeft (behalve een blad), dan zal die node meteen verwijderd moeten worden.

delete :: RBTree -> Number -> RBTree
delete (Leaf c) _ = Leaf c
delete (Node c n t1 t2@(Leaf _)) n'
  | n == n'   = Leaf Black
  | otherwise = Node c n (delete t1 n') t2
delete (Node c n t1 t2) n'
  | n == n'   = Node c n'' t1 t2'
  | otherwise = Node c n (delete t1 n') (delete t2 n')
  where
    n'' = leftmostValue t2
    t2' = removeLeftmostNode t2

balancedDelete :: RBTree -> Number -> RBTree
balancedDelete t n = grayBalance (delete t n)

