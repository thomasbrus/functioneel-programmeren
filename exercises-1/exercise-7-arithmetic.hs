import FPPrac

r :: (Num a) => a -> a -> [a]
r s v = [s] ++ (r (s + v) v)

r1 :: Number -> [a] -> a
r1 0 (x:xs) = x
r1 n (x:xs) = r1 (n - 1) xs

totaal :: Number -> Number -> [Number] -> Number
totaal i j a = sum (take (j - i + 1) (drop i a))