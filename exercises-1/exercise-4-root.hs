import FPPrac

wortel1 :: Number -> Number -> Number -> Number
wortel1 a b c = (-b - sqrt(discr a b c)) / (2 * a)

wortel2 :: Number -> Number -> Number -> Number
wortel2 a b c = (-b + sqrt(discr a b c)) / (2 * a)

discr :: Number -> Number -> Number -> Number
discr a b c   | y < 0       = error "discriminant negatief"
              | otherwise   = y
              where y = b ^ 2 - 4 * a * c
