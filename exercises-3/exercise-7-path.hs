-- Opgave 7. Een pad in een binaire boom is een string die
-- bestaat uit de letters ’l’ en ’r’, respectievelijk voor links
-- en rechts. Begin bij de wortel van de boom, en ga naar de linker
-- (resp. rechter) subboom als de eerstvolgende letter in het pad een
-- ’l’ (resp, ’r’) is. Een pad “wijst” dus naar de wortel van een subboom.

-- Ga bij deze opgave uit van bomen van type Tree1a.
-- a. Schrijf een functie vervang die in een gegeven een boom t het
-- getal bij een knoop (aangewezen door een pad p) vervangt door
-- een gegeven getal n.

import RoseTree
import FPPrac
import PreProcessor

myTree = Node1a 4
  (Node1a 1
    (Node1a 0
      (Leaf1a 9)
      (Leaf1a 13)
    )
    (Node1a 2
      (Leaf1a 6)
      (Leaf1a 3)
    )
  )
  (Node1a 5
    (Node1a 3
      (Leaf1a 11)
      (Leaf1a 12)
    )
    (Node1a 8
      (Leaf1a 12)
      (Leaf1a 15)
    )
  )

replaceValue :: String -> Number -> Tree1a -> Tree1a
replaceValue [] x (Leaf1a n)            = Leaf1a x
replaceValue [] x (Node1a n t1 t2)      = Node1a x t1 t2
replaceValue (d:ds) x (Node1a n t1 t2)  | d == 'l'    = Node1a n (replaceValue ds x t1) t2
                                        | d == 'r'    = Node1a n t1 (replaceValue ds x t2)

--main = showTreeList
--  [ preProcess1a myTree
--  , preProcess1a (replaceValue "lrl" 10 myTree)
--  ]

-- b. Schrijf een funktie subboom die, gegeven een pad en gegeven
-- een boom, de subboom oplevert die door het pad wordt aangewezen.
-- Als het pad te lang is, moet de funktie een error opleveren.

subTree :: String -> Tree1a -> Tree1a
subTree [] t                    = t
subTree ds (Leaf1a n)           = error "Path is too long"
subTree (d:ds) (Node1a n t1 t2) | d == 'l'  = subTree ds t1
                                | d == 'r'  = subTree ds t2

--main = showTreeList
--  [ preProcess1a myTree
--  , preProcess1a (subTree "lr" myTree)
--  ]

-- c – toegift. Een blad is buur van een ander blad als er geen
-- andere bladen tussen zitten. Een blad in een boom kan worden
-- aangeduid door een pad naar dat blad. Schrijf twee functies
-- linkerbuur en rechterbuur die gegeven een boom, en gegeven
-- een blad in die boom (aangeduid door een pad) de linker, resp.
-- de rechter buur van dat blad opleveren (eveneens in de vorm van het pad).
-- Bij het meest linker en meest rechter blad moet zo nodig een foutmelding
-- worden gegeven.
-- Maak het resultaat van uw functies grafisch zichtbaar door -1 bij de
-- buurbladeren van een gegeven blad te zetten.

