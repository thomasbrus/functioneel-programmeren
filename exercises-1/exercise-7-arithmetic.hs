import FPPrac

-- Schrijf een recursieve functie r die de
-- bijbehorende rekenkundige rij genereert.
-- Het type van r i s dus: r :: Num a => a -> a -> [a].

r :: (Num a) => a -> a -> [a]
r s v = [s] ++ (r (s + v) v)

r1 :: Number -> [a] -> a
r1 0 (x:xs) = x
r1 n (x:xs) = r1 (n - 1) xs

-- – Laat i en j twee indices zijn (bedenk dat
-- het eerste element uit een lijst index 0 heeft).
-- Schrijf een functie totaal die de som bepaalt
-- van het i-de element t/m het j-de element uit
-- diezelfde rij (gebruik r, take, en drop).

totaal :: Number -> Number -> [Number] -> Number
totaal i j a = sum (take (j - i + 1) (drop i a))

{-|
  *Main> (r 3 2)
  [3,5,7,9,11,13, ...]

  *Main> r1 0 (r 3 2)
  3
  *Main> r1 3 (r 3 2)
  9
  *Main> r1 1 (r 3 2)
  5

  *Main> totaal 1 1 [1,2,3,4,5,6]
  2
  *Main> totaal 0 0 [1,2,3,4,5,6]
  1
  *Main> totaal 0 1 [1,2,3,4,5,6]
  3
  *Main> totaal 1 3 [1,2,3,4,5,6]
  9
-}