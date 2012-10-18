module Main where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Graphics

import System.FilePath (splitPath, dropExtension)

import CreateGraph
import Debug.Trace

import Data.Maybe
import Data.List

data MyStore = MyStore
  { myGraph :: Graph
  , lastKeyPressed :: (Maybe Char)
  }

initPrac6 graph = MyStore { myGraph = graph, lastKeyPressed = Nothing }

main = doGraph doPrac6 initPrac6 myGraph drawMypracBottomLine

doPrac6 :: MyStore -> Input -> (MyStore, [Output])

doPrac6 myStore (KeyIn 'y') = resetAll myStore
doPrac6 myStore (KeyIn c) = (myStore { lastKeyPressed = Just c }, [])

doPrac6 myStore@(MyStore {myGraph=g, lastKeyPressed=k}) (MouseDown (mx, my))
  | isJust clickedNode
  = case fromJust k of
      'r' -> highlightCurrent myStore (fromJust clickedNode)
      't' -> highlightNeighbours myStore (fromJust clickedNode)
      _   -> (myStore, [])
  | otherwise
  = (myStore, [])
  where
    clickedNode = onNode (nodes g) (mx, my) 

doPrac6 myStore i = (myStore, [])

highlightCurrent :: MyStore -> Node -> (MyStore, [Output])
highlightCurrent s n@(c, _, p) =
  let
    g = myGraph s
    currentNode = (c, red, p)
    nodes' = currentNode : (delete n (nodes g))                    
    myGraph' = g { nodes = nodes' }
  in
  ( s { myGraph = myGraph' },
    [ DrawOnBuffer True
    , DrawPicture $ Pictures [drawGraph myGraph']
    ]
  )

highlightNeighbours :: MyStore -> Node -> (MyStore, [Output])
highlightNeighbours s n = 
  let
    g = myGraph s
    neighbours = findDirectNeighbours g n
    nodes' = map (\n@(c, _, p) -> if elem n neighbours then (c, blue, p) else n) (nodes g)
    myGraph' = g { nodes = nodes' }
  in
    ( s { myGraph = myGraph' },
      [ DrawOnBuffer True
      , DrawPicture $ Pictures [drawGraph myGraph']
      ]
    )

resetAll :: MyStore -> (MyStore, [Output])
resetAll s =
  let
    g = myGraph s
    nodes' = map (\(c, _, p) -> (c, white, p)) (nodes g)
    edges' = map (\(c1, c2, _, w) -> (c1, c2, black, w)) (edges g)
    myGraph' = g { nodes = nodes', edges = edges' }
  in
    ( s { myGraph = myGraph' },
      [ DrawOnBuffer True
      , DrawPicture $ Pictures [drawGraph myGraph']
      ]
    )

drawMypracBottomLine :: Graph -> Picture
drawMypracBottomLine graph =
  Pictures
    [ Translate 0 (-300 + bottomLineHeight / 2) $ Color white $ rectangleSolid 800 bottomLineHeight
    , Color black $ Line [(-400,height1),(400,height1)]
    , Color black $ Line [(-240,height1),(-240,-300)]
    , Translate (-392) height2 $ Color black $ Scale 0.11 0.11 $ Text "myprac:"
    , Translate (-332) height2 $ Color red   $ Scale 0.11 0.11 $ Text $ (case (name graph) of "" -> "" ; xs -> dropExtension $ last $ splitPath xs)
    -- Vervang onderstaande tekst, indien nodig, door extra informatie
    , Translate (-235) height2 $ Color black $ Scale 0.11 0.11 $ Text "Press 'q' to return to node-drawing"
    ]
    where
      height1 = -300 + bottomLineHeight
      height2 = -300 + bottomTextHeight
