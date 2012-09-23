-- – Schrijf een functie allEqual die nagaat
-- of alle elementen van een lijst gelijk zijn.

import FPPrac

allEqual :: (Eq a) => [a] -> Bool
allEqual (x:[]) = True
allEqual (x:xs) = (x == head xs) && allEqual xs

isRR :: [Number] -> Bool
isRR (x:[y,z]) = y - x == z - y
isRR (x:xs) = (head xs) - x == (head (tail xs)) - (head xs) && isRR xs

{-|
  *Main> allEqual [2,2,2]
  True
  *Main> allEqual [2,1,2]
  False
  *Main> allEqual [2,1,1]
  False
  *Main> allEqual []
  *** Exception: check.hs:(7,1)-(8,47): Non-exhaustive patterns in function allEqual
  *Main> allEqual [1]
  True

  *Main> isRR [1,2,3,4]
  True
  *Main> isRR [1,2,3,5]
  False
  *Main> isRR [-1,-4,-7,-11]
  False
  *Main> isRR [-1,-4,-7,-10]
  True
-}