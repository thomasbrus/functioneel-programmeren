-- Opgave 8. Het min-max algoritme neemt het minimum en het maximum uit de
-- lijst en zet die vooraan en achteraan (respectievelijk). Dit proces wordt
-- herhaald voor de lijst waaruit beide zijn verwijderd. Schrijf een functie
-- mmsort die dit algoritme implementeert (de Haskell module List kent een operatie 
-- die twee lijsten van elkaar aftrekt. Ga zelf na hoe die operatie precies werkt).

import FPPrac
import Data.List

minMaxSort :: Ord a => [a] -> [a]
minMaxSort [] = []
minMaxSort xs = [a] ++ (minMaxSort (xs \\ [a, b])) ++ [b]
                where a = minimum xs
                      b = maximum xs