module RBRun where
-- Grafische weergave van rood-zwart bomen. Werkt alleen voor *binaire* bomen.
--
-- Jan Kuper, 5 mei 2008
-- ============================================================================

import FPPrac.Events
import FPPrac.Graphics
import RBGraphics
import Prelude
import RBTree hiding (exampleTree)
import qualified RBTree as RBTree (exampleTree)

-- ============= types ========================================================
-- RBnode c v ts:   c=colour, v=value, ts=subtrees

data StateTp = StateTp { mode :: Bool
                       , rbts :: [RbTreeG]
                       }

initstate = StateTp { mode = False
                    , rbts = [ ppTree insertedTree, ppTree deletedTree ]
                    }
                    where
                      -- insertedTree = foldl balancedInsert (Leaf Black) [1..10]
                      insertedTree = balancedInsert RBTree.exampleTree 14
                      deletedTree  = balancedDelete RBTree.exampleTree 17

main = installEventHandler "RBrun" doE initstate (drawTrees m 200 ts) 25
   where
      StateTp { mode = m, rbts = ts} = initstate

---- ============= event handler ================================================

doE :: StateTp -> Input -> (StateTp, [Output])
doE s (KeyIn 'm') = (s {mode  = not (mode s)}, [ScreenClear , DrawPicture (drawTrees (not (mode s)) 200 (rbts s))])
doE s e           = (s, [])

-- ======voorbeeldboom=========================================================
-- Let op: deze boom is slechts ter illustratie van de grafische weergave,
--         hij voldoet *niet* aan de rood-zwart eis

exampleTree = RBnode black "9t"
                     [ RBnode red "99"
                              [ RBnode red "99"
                                       [ RBnode black "9"  []
                                       , RBnode black  "99" []
                                       ]
                              , RBnode red "ii"
                                       [ RBnode black "99"  []
                                       , RBnode black "9" []
                                       ]
                              ]
                     , RBnode red "k"
                              [ RBnode black "ll" [],
                                RBnode black  "m"
                                       [ RBnode red "nn"
                                                [ RBnode red "q" [ RBnode black "nn" []
                                                                , RBnode black "q"  []
                                                                ]
                                                , RBnode red "r" []
                                                ]
                                       , RBnode red "pp"
                                                [ RBnode black "r" [ RBnode red "nn" []
                                                               , RBnode  (dark $ dark white)  "" []
                                                               ]
                                                , RBnode black "r" [ RBnode red "nn" []
                                                               , RBnode  (dark white)  "" []
                                                               ]
                                                ]
                                       ]
                              ]
                     ]
