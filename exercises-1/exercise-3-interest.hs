--Opgave 3. Schrijf een recursieve functie die uitrekent hoeveel
--geld je na n jaren hebt als je begint met een bedrag b, en r
--procent rente per jaar krijgt. Hou rekening met rente over
--rente (die hoeft maar  ́e ́en keer per jaar te worden berekend).
--Laat n van type Int zijn, en b en r van type Float.

import FPPrac

rente :: Number -> Number -> Number -> Number

rente 0 b r = b
rente n b r = rente (n - 1) y r where y = b + b * r

{-|
  *Main> rente 1 100 0.03
  103.0
  *Main> rente 2 100 0.03
  106.09
  *Main> rente 3 100 0.03
  109.2727
-}