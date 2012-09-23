import FPPrac

bubbleOnce :: Ord a => [a] -> [a]
bubbleOnce (x:[]) = [x]
bubbleOnce (x:xs) | x > head xs   = head xs : bubbleOnce (x : (tail xs))
                  | otherwise     = x : bubbleOnce xs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs | y == xs     = xs
              | otherwise   = (bubbleSort (init y)) ++ [last y]
              where y = bubbleOnce xs