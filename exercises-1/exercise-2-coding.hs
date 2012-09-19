import Data.Char
import Prelude (Int)

codeer :: Int -> Char -> Char
codeer n c  | (c >= 'a' && c <= 'w') ||
              (c >= 'A' && c <= 'W')     = chr (ord c + n)
            | (c >= 'x' && c >= 'z') ||
              (c >= 'X' && c >= 'Z')     = chr (ord c + n - 26)
            | otherwise                  = c


{-|
  *Main> map codeer "hallo"
  "kdoor"
  *Main> map codeer "Morgenavond, 8 uur in Scheveningen"
  "Prujhqdyrqg, 8 xxu lq Vfkhyhqlqjhq"

  *Main> map (codeer 1) "hallo"
  "ibmmp"
-}