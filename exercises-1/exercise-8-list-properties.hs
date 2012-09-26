import FPPrac

allEqual :: (Eq a) => [a] -> Bool
allEqual [x] = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

isRR :: [Number] -> Bool
isRR (x:[y,z])  = y - x == z - y
isRR (x:y:z:xs) = y - x == z - y && isRR (y:z:xs)
