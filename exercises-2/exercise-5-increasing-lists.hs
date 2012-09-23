import FPPrac

isIncreasing :: [Number] -> Bool
isIncreasing (x:[]) = True
isIncreasing (x:xs) = x < (head xs) && isIncreasing xs

average :: [Number] -> Number
average l = (sum l) / (length l)

isWeaklyIncreasing :: [Number] -> Bool
isWeaklyIncreasing (x:[]) = True
isWeaklyIncreasing l = average (init l) < (last l) && isWeaklyIncreasing (init l)