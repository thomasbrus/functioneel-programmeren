{-# LANGUAGE ScopedTypeVariables #-}

import FPPrac
import BinTree
import Data.Either
import Data.Char

-- $ ghci exercises-4/exercise-2-parser.hs  exercises-3/RoseTree.hs exercises-4/exercise-1-bin-tree.hs                                  
-- eval' assign' "(a*3)"
-- eval' assign'' "(a*3)"

--26
--(3+5)
--(12.5 * (13 + 14.74 ))


-- E -> ( E O E )
-- E -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- O -> + | - | * | / | ^

-- spaties komen niet voor, en getal bestaat uit maar  ́e ́en cijfer,

-- context free grammars

data NonTerminal = E | O | P deriving Show

parse :: NonTerminal -> String -> (BinTree Char Number, String)
parse E (x:xs)  | isDigit x             = (Leaf (read [x] :: Number), xs)
                | x == '(' && r == ')'  = ((Node (head r1) t1 t3), rs)
                | otherwise             = error ("Parse error at '" ++ [x])

                where
                  (t1, r1) = parse E xs
                  (t2, r2) = parse O r1
                  (t3, r3) = parse E r2
                  (r:rs)   = r3

parse O (x:xs)  | x `elem` "+-*/^" = (Unit, xs)
                | otherwise = error ("Parse error: expected operator but found " ++ [x])


-- idem, maar nu kan een expressie ook variabelen bevatten
-- die uit  ́e ́en enkele letter bestaan (hint: gebruik het
-- Either type in Haskell, zie http://www.haskell.org/hoogle/?hoogle=either),

parse' :: NonTerminal -> String -> (BinTree Char (Either Char Number), String)
parse' E (x:xs) | isDigit x             = (Leaf (Right (read [x] :: Number)), xs)
                | isAlpha x             = (Leaf (Left x), xs)
                | x == '(' && r == ')'  = ((Node (head r1) t1 t3), rs)
                | otherwise             = error ("Parse error at " ++ [x])

                where
                  (t1, r1) = parse' E xs
                  (t2, r2) = parse O r1
                  (t3, r3) = parse' E r2
                  (r:rs)   = r3

-- er mogen spaties in een expressie staan, een getal kan
-- bestaan uit meerdere cijfers, en een variabele uit meerdere
-- letters (neem aan dat er alleen letters in een variabele
-- voorkomen). Gebruik finite state automatons voor het herkennen
-- van getallen en variabelen,

-- E    -> ( E O E )
-- D    -> N | V
-- V    -> a | b | c ... | V
-- N    -> N' | N''N'
-- N'   -> 0 | 1 | 2 ...
-- N''  -> 1 | 2 | 3 ...
-- O    -> + | - | * | / | ^
-- S    -> _S | _

parse'' :: NonTerminal -> String -> (BinTree Char (Either String Number), String)
parse'' E (x:xs)  | isDigit y             = (Leaf (Right ds), r4)
                  | isAlpha y             = (Leaf (Left as), r5)
                  | y == '(' && r == ')'  = ((Node (head r1) t1 t3), rs)
                  | otherwise             = error ("Parse error at " ++ [y])

                where
                  (y:ys)    = filter (\x -> False == isSpace x) (x:xs)
                  (t1, r1)  = parse'' E ys
                  (t2, r2)  = parse O r1
                  (t3, r3)  = parse'' E r2
                  (r:rs)    = r3
                  (ds, r4)  = parseNumber (y:ys)
                  (as, r5)  = parseVariable (y:ys)

parseNumber :: String -> (Number, String)
parseNumber xs  = (read ds :: Number, rs)
                where
                  ds = takeWhile isDigit xs
                  rs = dropWhile isDigit xs

parseVariable :: String -> (String, String)
parseVariable xs  = (as, rs)
                where
                  as = takeWhile isAlpha xs
                  rs = dropWhile isAlpha xs


-- (5) Schrijf een programma eval dat de waarde van een expressie uitrekent
-- door de expressie te parsen en vervolgens de resulterende expressie- boom
-- te evalueren. Om die waarde te kunnen uitrekenen, moet u een functie
-- assign defini ̈eren die aan elke variable in de expressie een waarde geeft. Dus:
-- in opdracht (1) is nog geen functie assign nodig
-- in opdracht (2): assign :: Char -> Number
-- in opdrachten (3) en (4): assign :: String -> Number
-- De functie eval moet nu gegeven een functie assign de waarde van een expressie
-- uitrekenen. Met andere woorden, assign is het eerste argument van de functie
-- eval (behalve bij opdracht (1), uiteraard).

--eval :: String -> Number
--eval xs = t1
--        where
--          (Node ) = parse'' E xs

eval :: String -> Number
eval xs = evalTree t
          where
            (t, s) = parse'' E xs
            evalTree (Node '+' t1 t2) = (evalTree t1) + (evalTree t2)
            evalTree (Node '-' t1 t2) = (evalTree t1) - (evalTree t2)
            evalTree (Node '*' t1 t2) = (evalTree t1) * (evalTree t2)
            evalTree (Node '/' t1 t2) = (evalTree t1) / (evalTree t2)
            evalTree (Node '^' t1 t2) = (evalTree t1) ^ (evalTree t2)
            evalTree (Leaf (Right v)) = v

eval' :: (String -> Number) -> String -> Number
eval' f xs = evalTree t
          where
            (t, s) = parse'' E xs
            evalTree (Node '+' t1 t2) = (evalTree t1) + (evalTree t2)
            evalTree (Node '-' t1 t2) = (evalTree t1) - (evalTree t2)
            evalTree (Node '*' t1 t2) = (evalTree t1) * (evalTree t2)
            evalTree (Node '/' t1 t2) = (evalTree t1) / (evalTree t2)
            evalTree (Node '^' t1 t2) = (evalTree t1) ^ (evalTree t2)
            evalTree (Leaf (Right v)) = v
            evalTree (Leaf (Left v)) = f v

assign' :: String -> Number
assign' "a" = 1

assign'' :: String -> Number
assign'' "ab" = 2

