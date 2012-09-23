import FPPrac
import Data.List

minMaxSort :: Ord a => [a] -> [a]
minMaxSort [] = []
minMaxSort xs = [a] ++ (minMaxSort (xs \\ [a, b])) ++ [b]
                where a = minimum xs
                      b = maximum xs