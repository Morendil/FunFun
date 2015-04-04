import Check (..)
import Random
import Merlin (gridSize)

import Text (plainText)

randomDimensions = Random.pair (Random.float 1 2048) (Random.float 1 2048)

-- Display a square grid centered in the window, taking up 2/3
-- intended use: outlined solid white <| rect gridSize

tests =
  simpleCheck [
    property "Main Grid Dimensions" (\(w,h) -> gridSize (w,h) == (2*(min w h)/3,2*(min w h)/3) ) randomDimensions
  ]

main = display tests