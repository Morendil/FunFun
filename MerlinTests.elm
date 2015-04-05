import Check (..)
import Random
import Merlin (gridSize,divide)

import Text (plainText)

randomDimensions = Random.pair (Random.float 1 2048) (Random.float 1 2048)

-- Display a square grid centered in the window, taking up 2/3
-- intended use: outlined solid white <| rect gridSize

tests =
  simpleCheck [
    property "Main Grid Dimensions" (\(w,h) ->
        let side = gridSize (w,h)
        in side == 2*(min w h)/3)
    randomDimensions,
    property2 "Divide grid" (\n (w,h) ->
        let small = divide n (gridSize (w,h))
            n' = toFloat n
        in small * n'+ 4 + 2 * (n'-1) == gridSize (w,h))
    (Random.int 1 10) randomDimensions
  ]

main = display tests