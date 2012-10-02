import FPPrac
import BinTree
import Data.Either

--26
--(3+5)
--(12.5 * (13 + 14.74 ))


-- E -> ( E O E )
-- E -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- O -> + | - | * | / | ^

-- spaties komen niet voor, en getal bestaat uit maar  ́e ́en cijfer,

-- context free grammars

data NonTerminal = E | O | P deriving Show

parse :: NonTerminal -> String -> (BinTree Char Char, String)
parse E (x:xs)  | x `elem` [ '0', '1', '2', '3', '4',
                             '5', '6', '7', '8', '9' ] = (Leaf x, xs)
                | otherwise = ((Node (head r2) t1 t3), r5)

                where
                  (p1, r1) = parse P (x:xs)
                  (t1, r2) = parse E r1
                  (t2, r3) = parse O r2
                  (t3, r4) = parse E r3
                  (p2, r5) = parse P r4

parse P []      = error "Parse error: unexpected end, expected paranthese."
parse P (x:xs)  | x `elem` [ '(', ')' ] = (Leaf x, xs)
                | otherwise = error ("Parse error: expected paranthese but found " ++ [x])


parse O []      = error "Parse error: unexpected end, expected operator."
parse O (x:xs)  | x `elem` [ '+', '-', '*'
                           , '/', '^' ] = (Leaf x, xs)
                | otherwise = error ("Parse error: expected operator but found " ++ [x])


-- idem, maar nu kan een expressie ook variabelen bevatten
-- die uit  ́e ́en enkele letter bestaan (hint: gebruik het
-- Either type in Haskell, zie http://www.haskell.org/hoogle/?hoogle=either),




