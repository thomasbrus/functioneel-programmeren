import FPPrac

mylength :: [a] -> Number
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mysum :: [Number] -> Number
mysum [] = 0
mysum (x:xs) = x + mysum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]

mytake :: Number -> [a] -> [a]
mytake n [] = []
mytake 1 (x:xs) = [x]
mytake n (x:xs) = x : mytake l xs where l = min (n - 1) (mylength xs)

myelem :: (Eq a) => a -> [a] -> Bool
myelem e [] = False
myelem e (x:xs) = (x == e) || (myelem e xs)

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: [Number] -> Number
mymaximum [x] = x
mymaximum (x:xs) = max x (mymaximum xs)

myzip :: [a] -> [b] -> [(a, b)]
myzip xs [] = []
myzip [] ys = []
myzip (x:xs) (y:ys) = [(x, y)] ++ myzip xs ys

