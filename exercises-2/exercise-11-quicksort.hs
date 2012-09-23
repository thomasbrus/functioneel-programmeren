import FPPrac

quicksort :: Ord a => [a] -> [a]
quicksort []      = []
quicksort (x:xs)  = (quicksort as) ++ x:bs ++ (quicksort cs)
                  where as = filter (< x) xs
                        bs = filter (== x) xs
                        cs = filter (> x) xs