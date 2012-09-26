import Data.Char
import FPPrac

import Data.Int

shiftCharacter :: Int -> Char -> Char
shiftCharacter n c  | (c >= 'a' && c <= 'z')     = chr ((ord c - (ord 'a') + n) `mod` 26 + ord 'a')
                    | (c >= 'A' && c <= 'Z')     = chr ((ord c - (ord 'A') + n) `mod` 26 + ord 'A')
                    | otherwise                  = c