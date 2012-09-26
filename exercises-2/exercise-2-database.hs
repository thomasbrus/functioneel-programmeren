import FPPrac
import Data.Char (toLower)
import Data.List (sort)

type Person = (String, Number, Char, String)
type PersonData = [Person]
people :: PersonData
people = 
  [ ("Person 1", 25, 'm', "Somewhere")
  , ("Person 2", 21, 'f', "Somewhere")
  , ("Person 3", 20, 'm', "Somewhere")
  , ("Person 4", 35, 'f', "Somewhere") ]

getName :: Person -> String
getName p = name where (name, _, _, _) = p

getAge :: Person -> Number
getAge p = age where (_, age, _, _) = p

getGender :: Person -> Char
getGender p = gender where (_, _, gender, _) = p

getPlace :: Person -> String
getPlace p = place where (_, _, _, place) = p

increaseAge :: Number -> PersonData -> PersonData
increaseAge n []      = []
increaseAge n (p:ps)  = (name, age + n, gender, place) : (increaseAge n ps)
                        where (name, age, gender, place) = p

increaseAge' :: Number -> PersonData -> PersonData
increaseAge' n ps = map (\(name, age, gender, place) -> (name, age + n, gender, place)) ps

increaseAge'' :: Number -> PersonData -> PersonData
increaseAge'' n ps = [ (name, age + n, gender, place) | (name, age, gender, place)<-ps ]

filterPersons :: PersonData -> [String]
filterPersons []      = []
filterPersons (p:ps)  | age >= 30 && age <= 40 && gender == 'f'  = name : filterPersons ps
                      | otherwise                                = filterPersons ps
                      where (name, age, gender, _) = p

filterPersons' :: PersonData -> [String]
filterPersons' ps = map (\(name, _, _, _) -> name)
                        (filter (\(name, age, gender, place) -> age >= 30 && age <= 40 && gender == 'f')
                         ps)

filterPersons'' :: PersonData -> [String]
filterPersons'' ps = [ name | (name, age, gender, _)<-ps, age >= 30, age <= 40, gender == 'f' ]

findAgeByName :: String -> PersonData -> Maybe Number
findAgeByName n []      = Nothing
findAgeByName n (p:ps)  | (map toLower name) == (map toLower n) = Just age
                        | otherwise                             = findAgeByName n ps
                        where (name, age, _, _) = p

sortByAge :: PersonData -> PersonData
sortByAge = map swap . sort . map swap
            where swap (name, age, gender, place) = (age, name, gender, place) 
