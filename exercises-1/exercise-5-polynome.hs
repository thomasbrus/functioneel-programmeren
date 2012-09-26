import FPPrac

extrX :: Number -> Number -> Number
extrX a b = -b / 2 * a

extrY :: Number -> Number -> Number -> Number
extrY a b c = f (extrX a b) where f x = a * x ^ 2 + b * x + c