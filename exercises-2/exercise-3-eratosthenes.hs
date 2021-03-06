import FPPrac

sieve :: [Number]
sieve = f [2..] where f (x:xs) = x : f (filter ((> 0) . (`mod` x)) xs)

isPrime :: Number -> Bool
isPrime n = n `elem` (primesSmallerThanN n)

firstNPrimes :: Number -> [Number]
firstNPrimes n = take n sieve

primesSmallerThanN :: Number -> [Number]
primesSmallerThanN n = (takeWhile (<=n) sieve)

divisors :: Number -> [Number]
divisors m = [ x | x <- [1..m], m `mod` x == 0]

isPrime2 :: Number -> Bool
isPrime2 m = [1, m] == (divisors m)