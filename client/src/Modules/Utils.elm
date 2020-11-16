
module Modules.Utils exposing (..)

import CodeGen.Spacemonkey as CSP
import Modules.Types exposing (..)

--------------------------------------------------------------------------------

moveDeltas : CSP.Direction -> (Int, Int)
moveDeltas dir = case dir of
                     CSP.North -> (0, -1)
                     CSP.South -> (0, 1)
                     CSP.East -> (1, 0)
                     CSP.West -> (-1, 0)
