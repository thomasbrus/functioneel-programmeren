--Opgave 1. In deze opgave moeten de binaire boomtypes van serie 3 gege-
-- neraliseerd worden tot een geparametriseerd boomtype
--BinTree a b

module BinTree where

import FPPrac
import Prelude
import RoseTree

-- a. Definieer het geparametriseerde boomtype BinTree a b
-- waarbij a het type van de elementen aan de interne knopen is,
-- en b het type van de elementen aan de bladeren.

data Unit = U
data BinTree a b  = Node a (BinTree a b) (BinTree a b) |
                    Leaf b | 
                    Unit 
                    deriving (Show)


-- b. Herdefinieer alle binaire boomtypes van serie 3 (1a t/m 1c)
-- als specifieke instanties van het type BinTree a b.
-- Voor type Tree1c (geen informatie aan de bladeren)
-- zult u een type Unit nodig hebben waarin maar een element
-- zit, om aan te geven dat er niets aan de bladeren staat.
-- Dit type moet u zelf definieren.

--data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a

type Tree1a = BinTree Number Number
type Tree1b = BinTree (Number, Number) (Number, Number)
type Tree1c = BinTree Number Unit

-- c. Schrijf een generieke functie pp die alle bomen
-- van type BinTree a b kan omzetten naar het type RoseTree
-- zodat ze grafisch getoond kunnen worden.

toRoseTree :: Show a => BinTree a a -> RoseTree
toRoseTree (Leaf n)       = RoseNode (show n) []
toRoseTree (Node n t1 t2) = RoseNode (show n) [(toRoseTree t1), (toRoseTree t2)]

--preProcess1a :: Tree1a -> RoseTree
--preProcess1a (Leaf1a n)       = RoseNode (show n) []
--preProcess1a (Node1a n t1 t2) = RoseNode (show n) [(preProcess1a t1), (preProcess1a t2)]

--preProcess1b (Leaf1b nt)        = RoseNode (show (nt)) []
--preProcess1b (Node1b nt t1 t2)  = RoseNode (show (nt)) [(preProcess1b t1), (preProcess1b t2)]


