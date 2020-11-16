module Modules.Camera exposing (..)

import CodeGen.Spacemonkey as CSP
import Modules.Types exposing (..)
import Modules.Utils as Utils

--------------------------------------------------------------------------------

initCam : ViewOpts -> ViewOpts
initCam vo =
    let ((x1, y1), end) = vo.camera
        start_ = (x1, y1)
        xCells = vo.windowWidth // vo.cellSize
        yCells = vo.windowHeight // vo.cellSize
        end_ = (x1 + xCells, y1 + yCells)
    in { vo | camera = (start_, end_) }

reinitCam : Int -> Int -> ViewOpts -> ViewOpts
reinitCam w h vo =
    let vo_ = {vo | windowWidth = w, windowHeight = h}
    in initCam vo_

moveCam : CSP.Direction -> ViewOpts -> ViewOpts
moveCam dir vo = let ((x1, y1), (x2, y2)) = vo.camera
                     (dx, dy) = Utils.moveDeltas dir
                in { vo | camera = ((x1+dx, y1+dy), (x2+dx, y2+dy)) }

getVisible : ViewOpts -> Grid -> Grid
getVisible vo grid =
    let ((x1, y1), (x2, y2)) = vo.camera
        visible cx cy = cx >= x1 && cx <= x2 && cy >= y1 && cy <= y2
    in List.filter (\c -> visible c.cellX c.cellY) grid
