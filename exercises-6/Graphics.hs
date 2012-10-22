module Graphics where

import Prelude
import FPPrac.Graphics
import System.FilePath (splitPath, dropExtension)
import Data.List
import Data.Maybe

data Thickness = Thin | Thick
  deriving (Eq,Show)

alphabet = ['a'..'z']

type Node  = (Char,Color,Point)
type Edge  = (Char,Char,Color,Int)

data Graph = Graph
  { name     :: String
  , directed :: Bool
  , weighted :: Bool
  , nodes    :: [Node]
  , edges    :: [Edge]
  } deriving (Eq,Show)

data EdgeDrawingData = EdgeDrawingData
  { edgeStartpunt   :: Point -- | (tx,ty): startpunt van edge
  , edgeEndpunt     :: Point -- | (px,py): eindpunt van edge
  , innerPijlpunt   :: Point -- | (qx,qy): binnenhoek van pijlpunt
  , achterPijlpunt  :: Point -- | (sx,sy): achterpunten van pijlpunt
  , breedtePijlpunt :: Point -- | (wx,wy): breedte pijlputn
  , edgeDikte       :: Point -- | (dx,dy): dikte van de edge
  , weightAfstand   :: Point -- | (ax,ay): afstand gewicht tot pijlpunt
  , weightMidden    :: Point -- | (mx,my): midden van edge  
  }

edgeWidthThin  = 1
edgeWidthThick = 1.5
nodeRadius     = 15
weightDistance = 6

arrowHeight    = 20
arrowDepth     = 9
arrowWidth     = 9

lblBoxW = 20
lblBoxH = 19


lblHshift      = 8
lblVshift      = -6

bottomLineHeight = 25
bottomTextHeight = 10

allEdgeDrawingData thickness graph (lbl1,lbl2,col,wght)
    = EdgeDrawingData
      { edgeStartpunt   = (tx,ty)                -- startpunt van edge
      , edgeEndpunt     = (px,py)                -- eindpunt van edge
      , innerPijlpunt   = (qx,qy)                -- binnenhoek van pijlpunt
      , achterPijlpunt  = (sx,sy)                -- achterpunten van pijlpunt
      , breedtePijlpunt = (wx,wy)                -- breedte pijlpunt
      , edgeDikte       = (dx,dy)                -- dikte van de edge
      , weightAfstand   = (ax,ay)                -- afstand gewicht tot pijlpunt
      , weightMidden    = (mx,my)                -- midden van edge
      }
    where
      Graph {directed=directed,nodes=nodes} = graph
      (x1,y1) = head [ (x,y) | (lbl,_,(x,y)) <- nodes, lbl==lbl1 ]
      (x2,y2) = head [ (x,y) | (lbl,_,(x,y)) <- nodes, lbl==lbl2 ]

      rico  = (y2-y1) / (x2-x1)
      alpha | x2 > x1              = atan rico
            | x2 == x1 && y2 > y1  = pi/2
            | x2 < x1              = pi + atan rico
            | x2 == x1 && y2 <= y1 = 3*pi/2
      sina  = sin alpha
      cosa  = cos alpha

      (xr1,yr1) = (nodeRadius * cosa , nodeRadius * sina)
      (tx ,ty ) = (x1+xr1,y1+yr1)                                               -- start of edge
      (px ,py ) = (x2-xr1,y2-yr1)                                               -- outer arrow point

      (xr2,yr2) = (arrowDepth * cosa , arrowDepth * sina)
      (qx,qy)   = (px-xr2,py-yr2)                                               -- inner arrow point

      (xh ,yh ) = (arrowHeight * cosa , arrowHeight * sina)
      (sx ,sy ) = (px-xh,py-yh)                                                 -- back arrowpoints

      (wx ,wy ) = (arrowWidth * sina , arrowWidth * cosa)                       -- width of arrowpoint

      (dx ,dy ) | thickness == Thick = (edgeWidthThick * sina , edgeWidthThick * cosa)
                | otherwise          = (edgeWidthThin  * sina , edgeWidthThin  * cosa) -- edge thickness

      (xwd,ywd) = (weightDistance * cosa , weightDistance * sina)
      (ax ,ay ) = (px-xwd,py-ywd)                                               -- distance of weight from arrowpoint

      (mx ,my ) = ((x2+x1)/2,(y2+y1)/2)                                       -- mid of (undirected) edge

drawNode :: Node -> Picture
drawNode (lbl,col,(x,y))
    = Pictures
      [ Translate x y $ Color col $ circleSolid r
      , Translate x y $ Color black $ Circle r
      , Translate (x-lblHshift) (y+lblVshift) $ Color black $ Scale 0.15 0.15 $ Text [lbl]
      ]
    where
      r = nodeRadius
      
drawEdgeTemp :: Color -> (Point,Point) -> Picture
drawEdgeTemp col (p1,p2)
  = Color col $ Line [p1,p2]
  
drawEdge :: Graph -> Edge -> Picture
drawEdge graph edge
    | directed =
      Pictures
      [ Color col   $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (tx+dx,ty-dy) ]
      , Color white $ Polygon [ (px+dx,py-dy), (px-dx,py+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (px+dx,py-dy) ]
      , Color col   $ Polygon [ (px,py), (sx-wx,sy+wy), (qx,qy), (sx+wx,sy-wy), (px,py) ]
      ]
    | otherwise =
      Color col $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (px-dx,py+dy), (px+dx,py-dy), (tx+dx,ty-dy) ]
    where
      Graph {directed=directed} = graph
      (_,_,col,_)               = edge

      EdgeDrawingData
        { edgeStartpunt   = (tx,ty)                -- startpunt van edge
        , edgeEndpunt     = (px,py)                -- eindpunt van edge
        , innerPijlpunt   = (qx,qy)                -- binnenhoek van pijlpunt
        , achterPijlpunt  = (sx,sy)                -- achterpunten van pijlpunt
        , breedtePijlpunt = (wx,wy)                -- breedte pijlpunt
        , edgeDikte       = (dx,dy)                -- dikte van de edge
        } = allEdgeDrawingData Thin graph edge
        
drawThickEdge :: Graph -> Edge -> Picture
drawThickEdge graph edge
    | directed =
      Pictures
      [ Color col   $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (tx+dx,ty-dy) ]
      , Color white $ Polygon [ (px+dx,py-dy), (px-dx,py+dy), (qx-dx,qy+dy), (qx+dx,qy-dy), (px+dx,py-dy) ]
      , Color col   $ Polygon [ (px,py), (sx-wx,sy+wy), (qx,qy), (sx+wx,sy-wy), (px,py) ]
      ]
    | otherwise =
      Color col $ Polygon [ (tx+dx,ty-dy), (tx-dx,ty+dy), (px-dx,py+dy), (px+dx,py-dy), (tx+dx,ty-dy) ]
    where
      Graph {directed=directed} = graph
      (_,_,col,_)               = edge

      EdgeDrawingData
        { edgeStartpunt   = (tx,ty)                -- startpunt van edge
        , edgeEndpunt     = (px,py)                -- eindpunt van edge
        , innerPijlpunt   = (qx,qy)                -- binnenhoek van pijlpunt
        , achterPijlpunt  = (sx,sy)                -- achterpunten van pijlpunt
        , breedtePijlpunt = (wx,wy)                -- breedte pijlpunt
        , edgeDikte       = (dx,dy)                -- dikte van de edge
        } = allEdgeDrawingData Thick graph edge
        
drawWeight :: Graph -> Edge -> Picture
drawWeight graph edge
  | not weighted = Blank
  | directed     = Pictures
    [ Translate ax ay $ Color white $ rectangleSolid lblBoxW lblBoxH
    , Translate (ax-lblHshift) (ay+lblVshift) $ Color black $ Scale 0.11 0.11 $ Text (show wght)
    ]
  | otherwise    = Pictures
    [ Translate mx my $ Color white $ rectangleSolid lblBoxW lblBoxH
    , Translate (mx-lblHshift) (my+lblVshift) $ Color black $ Scale 0.11 0.11 $ Text (show wght)
    ]
  where
    Graph {directed=directed,weighted=weighted}=graph
    (_,_,_,wght) = edge

    EdgeDrawingData
      { weightAfstand   = (ax,ay)                -- afstand gewicht tot pijlpunt
      , weightMidden    = (mx,my)                -- midden van edge
      }
      = allEdgeDrawingData Thin graph edge
      
drawGraph :: Graph -> Picture
drawGraph graph 
    = Pictures $
      Color white (rectangleSolid 800 564)
      :  (map drawNode nodes)
      ++ (map (drawEdge graph) edges)
      ++ (map (drawWeight graph) edges)
    where
      Graph {name=name,nodes=nodes,edges=edges}=graph

drawBottomLine :: Graph -> Picture
drawBottomLine graph
    = Pictures
      [ Translate 0 (-300 + bottomLineHeight / 2) $ Color white $ rectangleSolid 800 bottomLineHeight
      , Color black $ Line [(-400,height1),(400,height1)]
      , Color black $ Line [(-240,height1),(-240,-300)]
      , Color black $ Line [(100,height1),(100,-300)]
      , Translate (-392) height2 $ Color black $ Scale 0.11 0.11 $ Text "create:"
      , Translate (-332) height2 $ Color red   $ Scale 0.11 0.11 $ Text $ (case (name graph) of "" -> "" ; xs -> dropExtension $ last $ splitPath xs)
      , Translate (-235) height2 $ Color black $ Scale 0.11 0.11 $ Text "click: node; drag: edge; double: remove node"
      , Translate 120    height2 $ Color black $ Scale 0.11 0.11 $ Text "[n]ew; [r]ead; [s]ave; save [a]s; prac[6]"
      ]
    where
      height1 = -300 + bottomLineHeight
      height2 = -300 + bottomTextHeight


-- b. Definieer een commando waarmee alle directe buren van een met een
-- muisklik geselecteerde node blauw worden gekleurd. Doe dit zowel voor
-- gerichte als voor ongerichte grafen. Bij gerichte grafen mogen buren
-- alleen in de pijlrichting worden genomen.

-- *Graphics> findDirectNeighbours exampleGraph (exampleNodes !! 0)
-- [('b',RGBA 0.0 0.0 0.0 1.0,(1.0,2.0))]

-- *Graphics> findDirectNeighbours exampleGraph' (exampleNodes !! 1)
-- [('c',RGBA 0.0 0.0 0.0 1.0,(1.0,3.0)),('d',RGBA 0.0 0.0 0.0 1.0,(1.0,4.0))]

findDirectNeighbours :: Graph -> Node -> [Node]
findDirectNeighbours g n@(c, _, _) 
  | directed g
  = delete n $ filter (\(c, _, _) -> elem c cs') (nodes g)
  | otherwise
  = delete n $ filter (\(c, _, _) -> elem c cs) (nodes g)
  where
    cs  = concat $ map (\(c1, c2, _, _) -> [c1, c2]) $ filter (\(c1, c2, _, _) -> c1 == c || c2 == c) (edges g)
    cs' = concat $ map (\(c1, c2, _, _) -> [c1, c2]) $ filter (\(c1, c2, _, _) -> c1 == c) (edges g)

exampleNodes =
  [ ('a', black, (1, 1))
  , ('b', black, (1, 2))
  , ('c', black, (1, 3))
  , ('d', black, (1, 4))
  ]

exampleEdges =
  [ ('a', 'b', black, 1)
  , ('b', 'c', black, 2)
  , ('b', 'd', black, 3)
  ]

exampleGraph = Graph "exampleGraph" False False exampleNodes exampleEdges
exampleGraph' = exampleGraph { directed = True }

allEdges = 
  [ ('a', 'b', black, 1)
  , ('a', 'c', black, 1)
  , ('a', 'd', black, 1)
  , ('b', 'c', black, 1)
  , ('b', 'd', black, 1)
  , ('c', 'd', black, 1)
  ]

completeGraph = Graph "completeGraph" False False exampleNodes allEdges

partialEdges = 
  [ ('a', 'b', black, 1)
  , ('c', 'd', black, 1)
  ]

disconnectedGraph = Graph "disconnectedGraph" False False exampleNodes partialEdges

-- Opgave 2
-- a. Schrijf een functie waarmee getest kan worden of een ongerichte graaf
-- volledig is (d.w.z. er is een edge tussen elke node en elke andere node).
-- Deze test moet expliciet alle mogelijke bindingen langslopen (dus niet
-- alleen maar nodes en edges tellen).

-- *Graphics> isComplete disconnectedGraph 
-- False
-- *Graphics> isComplete exampleGraph
-- False
-- *Graphics> isComplete completeGraph 
-- True

isComplete :: Graph -> Bool
isComplete g = all (\n -> findDirectNeighbours g n == (nodes g) \\ [n]) (nodes g)

-- b. Schrijf een functie om te testen of een ongerichte graaf
-- samenhangend is, d.w.z. of er een pad loopt van elke node naar
-- elke andere node. Daarbij bestaat een pad uit een aantal edges achter elkaar.

-- *Graphics> isConnected exampleGraph
-- True
-- *Graphics> isConnected disconnectedGraph 
-- False
-- *Graphics> isConnected completeGraph 
-- True

isConnected :: Graph -> Bool
isConnected g = all (\n -> intersect (nodes g \\ [n]) (reachableNodes g n) == (nodes g \\ [n])) (nodes g)

removeEdge :: Graph -> Node -> Node -> Graph
removeEdge g n1 n2 =
  let
    (l1, _, _) = n1
    (l2, _, _) = n2
    edges' = filter (\(a, b, _, _) -> (a, b) /= (l1, l2) && (a, b) /= (l2, l1)) (edges g)
  in
    g { edges = edges' }

reachableNodes :: Graph -> Node -> [Node]
reachableNodes g n =
    let
      neighbours = findDirectNeighbours g n
    in
      union neighbours $ concat (map (\n' -> reachableNodes (removeEdge g n n') n') neighbours)

-- c. Schrijf een functie die de lijst van samenhangende subgrafen van een
-- gegeven ongerichte graaf oplevert (met “subgrafen” worden hier de grootst
-- mogelijke subgrafen bedoeld). Schrijf een commando waarmee die samen-
-- hangende subgrafen elk hun eigen kleur krijgen (dwz: alle nodes uit een
-- samenhangende subgraaf moeten dezelfde kleur krijgen. Uw functie mag zelf
-- beslissen welke kleuren daarvoor worden gekozen).

-- *Graphics> colorSubGraphs disconnectedGraph [red, blue, white, white]
-- Graph {name = "disconnectedGraph", directed = False, weighted = False, nodes = [('a',RGBA 1.0 0.0 0.0 1.0,(1.0,1.0)),('b',RGBA 1.0 0.0 0.0 1.0,(1.0,2.0)),('c',RGBA 0.0 0.0 1.0 1.0,(1.0,3.0)),('d',RGBA 0.0 0.0 1.0 1.0,(1.0,4.0))], edges = [('a','b',RGBA 0.0 0.0 0.0 1.0,1),('c','d',RGBA 0.0 0.0 0.0 1.0,1)]}

recolorNodes :: [Node] -> Color -> [Node]
recolorNodes ns color = map (\(c, _, p) -> (c, color, p)) ns

colorSubGraphs :: Graph -> [Color] -> Graph
colorSubGraphs g cs = g { nodes = concat $ map (\(ns, c) -> recolorNodes ns c) $ zip (groupBySubGraph g) cs  }

groupBySubGraph :: Graph -> [[Node]]
groupBySubGraph g
  | null (nodes g)
  = []
  | otherwise
  = let current = (head $ nodes g); reachables = reachableNodes g current
    in [current : reachables] ++ groupBySubGraph g { nodes = (tail $ nodes g) \\ reachables }

-- Opgave 3.
-- a. Schrijf een functie die test of in een gerichte graaf een gegeven node B
-- bereikbaar is (in overeenstemming met de edgerichting) vanuit een node A.

-- *Graphics> isReachable exampleGraph' (exampleNodes !! 1) (exampleNodes !! 0)
-- False
-- *Graphics> isReachable exampleGraph' (exampleNodes !! 0) (exampleNodes !! 1)
-- True

removeEdge' :: Graph -> Node -> Node -> Graph
removeEdge' g n1 n2 =
  let
    (l1, _, _) = n1
    (l2, _, _) = n2
    edges' = filter (\(a, b, _, _) -> (a, b) /= (l1, l2)) (edges g)
  in
    g { edges = edges' }

reachableNodes' :: Graph -> Node -> [Node]
reachableNodes' g n =
    let
      neighbours = findDirectNeighbours g n
    in
      union neighbours $ concat (map (\n' -> reachableNodes' (removeEdge' g n n') n') neighbours)

isReachable :: Graph -> Node -> Node -> Bool
isReachable g n1 n2 = n2 `elem` (reachableNodes' g n1)

-- b. Schrijf een functie die (in een gerichte graaf) de lijst van alle paden
-- van A naar B berekent. Daarbij is verondersteld dat een pad geen lussen bevat.
-- Definieer een commando dat één voor één die paden kleurt.

-- *Graphics> findPaths exampleGraph (exampleNodes !! 0) (exampleNodes !! 1)
-- [[('a',RGBA 0.0 0.0 0.0 1.0,(1.0,1.0)),('b',RGBA 0.0 0.0 0.0 1.0,(1.0,2.0))]]

findNodeByChar :: Graph -> Char -> Maybe Node
findNodeByChar g c = find (\(c', _, _) -> c == c') (nodes g)

findPaths :: Graph -> Node -> Node -> [[Node]]
findPaths g a b = findPaths' g a b []

findPaths' :: Graph -> Node -> Node -> [Node] -> [[Node]]
findPaths' g a@(a', _, _) b current = findPaths'' g a b current [ fromJust (findNodeByChar g y) | (x, y, _, _) <- (edges g), x == a' ]

findPaths'' :: Graph -> Node -> Node -> [Node] -> [Node] -> [[Node]]
findPaths'' g a b current []
  | a == b    = [current ++ [b]]
  | otherwise = []

findPaths'' g a b current (x:xs)
  | elem a current  = []
  | otherwise       = (findPaths' g x b (current ++ [a])) ++ (findPaths'' g a b current xs)

colorPaths :: Graph -> Node -> Node -> Color -> [[Node]]
colorPaths g a b c = map (\ns -> recolorNodes ns c) (findPaths g a b)

-- c. Definieer met behulp van deze functie een functie die in een 
-- gerichte en gewogen graaf het kortste pad van A naar B vindt en kleurt.
-- Hierbij betekent het begrip “kortst” dat het totaal van alle gewichten
-- op het pad minimaal moet zijn.

-- *Graphics> colorMinimalPath  exampleGraph' (exampleNodes !! 0) (exampleNodes !! 3) red
-- [('a',RGBA 1.0 0.0 0.0 1.0,(1.0,1.0)),('b',RGBA 1.0 0.0 0.0 1.0,(1.0,2.0)),('d',RGBA 1.0 0.0 0.0 1.0,(1.0,4.0))]

findEdgeByNodes :: Graph -> Node -> Node -> Maybe Edge
findEdgeByNodes g (a, _, _) (b, _, _) = find (\(a', b', _, _) -> a == a' && b == b') (edges g)

pathWeight :: Graph -> [Node] -> Int
pathWeight g (n:[]) = 0
pathWeight g (n:ns) = let (_, _, _, w) = fromJust (findEdgeByNodes g n (head ns)) in w + pathWeight g ns

findMinimalPath :: Graph -> Node -> Node -> [Node]
findMinimalPath g a b = minimumBy (\p1 p2 -> compare (pathWeight g p1) (pathWeight g p2)) (findPaths g a b)

colorMinimalPath :: Graph -> Node -> Node -> Color -> [Node]
colorMinimalPath g a b c = recolorNodes (findMinimalPath g a b) c

