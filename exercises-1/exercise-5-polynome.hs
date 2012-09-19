-- Een tweedegraads polynoom is van de vorm ax2 +bx+c
-- (neem weer aan dat a ̸= 0).
-- Schrijf een funktie extrX die bij gegeven a, b, c bepaalt bij
-- welke waarde van x deze polynoom zijn extreme waarde heeft.
-- Schrijf een funktie extrY die deze extreme waarde van de polynoom
-- bepaalt.

import FPPrac

extrX :: Number -> Number -> Number
extrX a b = -b / 2 * a

extrY :: Number -> Number -> Number -> Number
extrY a b c = f (extrX a b) where f x = a * x ^ 2 + b * x + c

{-|
  *Main> extrX 4 2
  -4.0
  *Main> extrY 4 2 1
  57.0
|}