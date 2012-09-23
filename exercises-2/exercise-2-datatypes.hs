-- Opgave 2. Van personen zijn opgeslagen in een database: naam,
-- leeftijd, geslacht, woonplaats. Neem aan dat personen uniek zijn
-- bepaald door hun naam.
-- Deelopgaven f, g hoeven maar op een van deze manieren te worden gemaakt.

import FPPrac

-- a. Geef het type van zo’n database, waarbij de gegevens van een
-- persoon in een 4-tupel worden opgeslagen. Maak zelf een
-- voorbeeld database van dit type om onderstaande functies te
-- kunnen testen.

--type Person = (Number, String, Number, Char, String)
--type PersonData = [Person]
--people :: PersonData
--people = 
--  [ (1, "Person 1", 20, 'm', "Somewhere")
--  , (2, "Person 2", 21, 'f', "Somewhere")
--  , (3, "Person 3", 22, 'm', "Somewhere")
--  , (4, "Person 4", 23, 'f', "Somewhere") ]

data Person = Pers { name::String, age::Number, gender::Char, place::String } deriving Show
type Persons = [Person]



-- b. Schrijf functies om de afzonderlijke gegevens van 
-- een persoon uit een tupel te kunnen halen.
--findById :: Number -> [String]
--findById id = [name | (i, name, age, gender, place)<-people, i == id]

-- c. Schrijf op drie verschillende manieren (met recursie,
-- met lijstcomprehensie, en met hogere orde functies) een
-- functie om de leeftijd van alle personen met n jaar op te hogen.

--increaseAge :: Number -> PersonData -> PersonData
--increaseAge n (x:xs) = (id, name, age + n, gender, place)<-x : increaseAge n xs

