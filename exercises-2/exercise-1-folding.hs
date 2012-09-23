import FPPrac

myfilter :: (a->Bool) -> [a] -> [a]
myfilter p (x:xs) | p x         = [x] ++ myfilter p xs
                  | otherwise   = myfilter p xs

myfoldl :: (a->b->a) -> a -> [b] -> a
myfoldl f v (x:[]) = f v x
myfoldl f v (x:xs) = myfoldl f (f v x) xs

myfoldr :: (a->b->a) -> a -> [b] -> a
myfoldr f v (x:[]) = f v x
myfoldr f v l = myfoldr f (f v (last l)) (init l)

myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f (x:[]) (y:[]) = [f x y]
myzipWith f (x:xs) (y:ys) = (f x y) : myzipWith f xs ys