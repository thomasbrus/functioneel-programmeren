--Schrijf twee funkties wortel1 en wortel2 die (gegeven a, b, c)
--de wortels bepalen van de vierkantsvergelijking ax2 + bx + c = 0
--(neem aan dat a ̸= 0). Geef als waarde van deze funkties bij
--negatieve discriminant:

import FPPrac

wortel1 :: Number -> Number -> Number -> Number
wortel1 a b c = (-b - sqrt(discr a b c)) / (2 * a)

wortel2 :: Number -> Number -> Number -> Number
wortel2 a b c = (-b + sqrt(discr a b c)) / (2 * a)

discr :: Number -> Number -> Number -> Number
discr a b c   | y < 0       = error "discriminant negatief"
              | otherwise   = y
              where y = b ^ 2 - 4 * a * c

{-|
  *Main> discr 4 10 1
  84
  *Main> wortel1 4 10 1
  -11.75
  *Main> wortel2 4 10 1
  9.25
  *Main> wortel1 4 1 1
  *** Exception: discriminant negatief
-}
