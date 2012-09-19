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
mytake n (x:xs) = [x] ++ mytake l xs where l = min (n - 1) (mylength xs)

myelem :: (Eq a) => a -> [a] -> Bool
myelem e [] = False
myelem e (x:xs) = (x == e) || (myelem e xs)

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: [Number] -> Number
mymaximum [x] = x
mymaximum (x:xs) = max x (mymaximum xs)

-- [[1,2,3,4],[4,3,2,1]] -> [[1,4],[2,3],[3,2],[4,1]]
myzip :: [a] -> [b] -> [(a, b)]
myzip [] [] = []
myzip (x:xs) [] = []
myzip [] (y:ys) = []
myzip (x:xs) (y:ys) = [(x, y)] ++ myzip xs ys

{-|
  *Main> mylength [1,2,3]
  3
  *Main> mylength []
  0

  *Main> mysum [1,2,3]
  6
  *Main> mysum [3,3,3]
  9

  *Main> myreverse [3,3,4,1]
  [1,4,3,3]
  *Main> myreverse [2,3,4,1]
  [1,4,3,2]


  *Main> mytake 0 [1,2,3,4]
  [1,2,3,4]
  *Main> mytake 1 [1,2,3,4]
  [1]
  *Main> mytake 2 [1,2,3,4]
  [1,2]
  *Main> mytake 5 [1,2,3,4]
  [1,2,3,4]
  *Main> mytake 5 []
  []
  *Main> mytake 0 [1,2]
  [1,2]

  *Main> myelem 3 [1,2,3]
  True
  *Main> myelem 4 [1,2,3]
  False

  *Main> myconcat [[1,2],[3,4],[4]]
  [1,2,3,4,4]
  *Main> myconcat []
  []

  *Main> mymaximum [1,2,3,4]
  4
  *Main> mymaximum [1,22,3,4]
  22

  *Main> myzip [1,2,3,4] [4,3,2]
  [(1,4),(2,3),(3,2)]
  *Main> myzip [1,2,3,4,5,6] [4,3,2]
  [(1,4),(2,3),(3,2)]
  *Main> myzip [1,2,3,4,5,6] [4,3,2,5]
  [(1,4),(2,3),(3,2),(4,5)]
  *Main> myzip [1,2,3,4,5,6] [4,3,2,5,5,5]
  [(1,4),(2,3),(3,2),(4,5),(5,5),(6,5)]
-}

