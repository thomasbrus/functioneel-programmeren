import FPPrac

isIncreasing :: [Number] -> Bool
isIncreasing (x:[]) = True
isIncreasing (x:xs) = x < (head xs) && isIncreasing xs

average :: [Number] -> Number
average xs = (sum xs) / (length xs)

isWeaklyIncreasing :: [Number] -> Bool
isWeaklyIncreasing (x:[]) = True
isWeaklyIncreasing xs = average (init xs) < (last xs) && isWeaklyIncreasing (init xs)