-- Opgave 9. Het algoritme insertion sort neemt bij het sorteren
-- van een lijst de lege lijst als uitgangspunt en voegt de
-- elementen van de te sorteren lijst een voor een op de juiste plaats toe.
-- Schrijf de functie isort (de Haskell module List kent al een functie
-- insert). Maak gebruik van een van de fold functies.

import FPPrac
import Data.List (insert)
 
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []