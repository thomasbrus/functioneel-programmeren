import FPPrac

rente :: Number -> Number -> Number -> Number

rente 0 b r = b
rente n b r = rente (n - 1) y r where y = b + b * r