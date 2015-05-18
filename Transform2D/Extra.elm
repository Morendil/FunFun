module Transform2D.Extra (transform,inverse,main) where

import Transform2D
import List
import Native.Transform2D.Extra

-- For debug
import Graphics.Element

transform : Transform2D.Transform2D -> (Float,Float) -> (Float,Float)
transform = Native.Transform2D.Extra.transform

inverse : Transform2D.Transform2D -> Transform2D.Transform2D
inverse = Native.Transform2D.Extra.inverse

gridSize = 9
size = 70

applyAll = List.foldl Transform2D.multiply Transform2D.identity

matrix =
    let offset = Transform2D.translation (-gridSize*size/2) (gridSize*size/2)
        aroundHorizontal = Transform2D.matrix 1 0 0 (cos (degrees 60)) 0 -(sin (degrees 60))
        aroundNormal = Transform2D.matrix (cos (degrees 45)) -(sin (degrees 45)) (sin (degrees 45)) (cos (degrees 45)) 0 0
    in applyAll [offset, aroundNormal, aroundHorizontal]

main = Graphics.Element.show <| toString <| transform matrix (50,50)