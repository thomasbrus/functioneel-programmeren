-- Opgave 4. Een drietal gehele getallen (a,b,c) heet een pythagoreısch
-- drietal, als a2 + b2 = c2. Bekende voorbeelden van dergelijke drietallen
-- zijn (3,4,5) en (5,12,13).
-- a. Schrijf een funktie pyth die alle pythagoreısche drietallen onder
-- een zeker maximum n genereert. Ga na of uw functie ook zou werken
-- als u dit maximum n weg zou laten uit de definitie.

import FPPrac

pyth :: Number -> [(Number, Number, Number)]
pyth x = [(a, b, c) | a <- [1..x], b <- [(a+1)..x], c <- [(b+1)..x], (a ^ 2) + (b ^ 2) == (c ^ 2)]

pyth2 = zeef2 . pyth
 where
   zeef2 ((a,b,c):xs) = (a,b,c) : zeef (filter (\(x,y,z) -> 
      (a `mod` x) /= 0 &&
      (b `mod` y) /= 0
    )  xs)

-- pyth' :: Number -> [(Number, Number, Number)]
-- pyth' x = [(a, b, c) | (a, b, c) <- (pyth x),
--                         a < b, b < c] -- [(3,4,5),(5,12,13),(6,8,10),(9,12,15)] ..?