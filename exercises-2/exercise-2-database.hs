-- Opgave 2. Van personen zijn opgeslagen in een database: naam,
-- leeftijd, geslacht, woonplaats. Neem aan dat personen uniek zijn
-- bepaald door hun naam.
-- Deelopgaven f, g hoeven maar op een van deze manieren te worden gemaakt.

import FPPrac
import Data.Char (toLower)

-- a. Geef het type van zo’n database, waarbij de gegevens van een
-- persoon in een 4-tupel worden opgeslagen. Maak zelf een
-- voorbeeld database van dit type om onderstaande functies te
-- kunnen testen.

type Person = (String, Number, Char, String)
type PersonData = [Person]
people :: PersonData
people = 
  [ ("Person 1", 20, 'm', "Somewhere")
  , ("Person 2", 21, 'f', "Somewhere")
  , ("Person 3", 22, 'm', "Somewhere")
  , ("Person 4", 35, 'f', "Somewhere") ]

-- b. Schrijf functies om de afzonderlijke gegevens van 
-- een persoon uit een tupel te kunnen halen.

getName :: Person -> String
getName p = name where (name, _, _, _) = p

getAge :: Person -> Number
getAge p = age where (_, age, _, _) = p

getGender :: Person -> Char
getGender p = gender where (_, _, gender, _) = p

getPlace :: Person -> String
getPlace p = place where (name, _, _, place) = p

-- c. Schrijf op drie verschillende manieren (met recursie,
-- met lijstcomprehensie, en met hogere orde functies) een
-- functie om de leeftijd van alle personen met n jaar op te hogen.

increaseAge :: Number -> PersonData -> PersonData
increaseAge n []      = []
increaseAge n (p:ps)  = (name, age + n, gender, place) : (increaseAge n ps)
                      where (name, age, gender, place) = p

-- d. Schrijf op dezelfde drie manieren een functie die de
-- namen oplevert van alle vrouwen tussen 30 en 40 jaar.

filterPersons :: PersonData -> [String]
filterPersons []      = []
filterPersons (p:ps)  | age >= 30 && age <= 40 && gender == 'f'  = name : filterPersons ps
                      | otherwise                                = filterPersons ps
                      where (name, age, gender, _) = p

--filterPersons' = [ p | (name, age, _, _)<-people && age >= 30 && age <= 40 ]

-- e. Schrijf een functie (een manier is voldoende) die de leeftijd van iemand
-- (gegeven door zijn/haar naam) oplevert. De naam moet met hoofd- en/of kleine
-- letters door elkaar geschreven kunnen worden (de Haskell module Char
-- kent de functies toLower, toUpper).

findAgeByName :: String -> PersonData -> Maybe Number
findAgeByName n []      = Nothing
findAgeByName n (p:ps)  | (map toLower name) == (map toLower n)   = Just age
                        | otherwise   = findAgeByName n ps
                        where (name, age, _, _) = p

-- Sorteer het bestand op leeftijd.
-- Haskell kent een standaardfunctie sort die ook voor tupels werkt (ga na hoe).
-- Schrijf een functie die een tupel “swapt”, en gebruik vervolgens hogere
-- orde functies en functiecompositie.

-- ....
