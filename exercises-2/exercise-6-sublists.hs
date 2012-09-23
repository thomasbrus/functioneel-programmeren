import FPPrac

isPartialList :: Eq a => [a] -> [a] -> Bool
isPartialList xs [] = False
isPartialList xs ys = (zip xs ys) == (zip xs xs) || isPartialList xs (tail ys)

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList [] _ = True
isSubList xs [] = False
isSubList (x:xs) ys = (elem x ys) && isSubList xs (tail (dropWhile (/=x) ys))