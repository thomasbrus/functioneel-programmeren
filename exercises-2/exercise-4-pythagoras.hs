-- Opgave 4. Een drietal gehele getallen (a,b,c) heet een pythagoreısch
-- drietal, als a2 + b2 = c2. Bekende voorbeelden van dergelijke drietallen
-- zijn (3,4,5) en (5,12,13).
-- a. Schrijf een funktie pyth die alle pythagoreısche drietallen onder
-- een zeker maximum n genereert. Ga na of uw functie ook zou werken
-- als u dit maximum n weg zou laten uit de definitie.

import FPPrac

pyth :: Number -> [(Number, Number, Number)]
pyth x = [(a, b, c) | a <- [1..x], b <- [1..x], c <- [1..x], (a ^ 2) + (b ^ 2) == (c ^ 2)]

pyth' :: Number -> [(Number, Number, Number)]
pyth' x = [(a, b, c) | (a, b, c) <- (pyth x),
                        a < b, b < c] -- [(3,4,5),(5,12,13),(6,8,10),(9,12,15)] ..?