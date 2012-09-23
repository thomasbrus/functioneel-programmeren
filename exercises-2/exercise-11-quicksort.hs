-- Opgave 11. Het quick sort algoritme verdeelt de lijst in elementen
-- die kleiner zijn dan het eerste element, gelijk zijn aan het eerste element,
-- en groter zijn dan het eerste element. Dit proces wordt herhaald voor de af-
-- zonderlijke lijsten. Schrijf de functie qsort.

import FPPrac

quicksort :: Ord a => [a] -> [a]
quicksort []      = []
quicksort (x:xs)  = (quicksort as) ++ [x] ++ (quicksort bs)
                  where as = filter (< x) xs
                        bs = filter (>= x) xs