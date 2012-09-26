import FPPrac

hasValidDimensions :: [[m]] -> Bool
hasValidDimensions (x:[]) = True
hasValidDimensions (x:xs) = length x == length (head xs) && hasValidDimensions xs

sumOfRows :: [[Number]] -> [Number]
sumOfRows (x:[]) = [sum x]
sumOfRows (x:xs) = [sum x] ++ sumOfRows xs

transpose ([]:_) = []
-- map head [[1,2,3],[4,5,6],[7,8,9]] => [1,4,7]
-- [1,2,3] : [4,5,6] : [] => [[1,2,3],[4,5,6]]
transpose x = (map head x) : transpose (map tail x)

sumOfColumns :: [[Number]] -> [Number]
sumOfColumns x = sumOfRows (transpose x)