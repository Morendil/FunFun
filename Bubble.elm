import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)
import Time exposing (..)
import Signal

shape = circle 50

total time dt = time + dt
time = Signal.foldp total 0 (fps 20)

main = Signal.map draw time

draw time = collage 500 500
        [filled red <| deform time shape]

deform time shape =
    indexedMap (reduce time) shape

reduce time index (x,y) =
    let findex = toFloat index
        zoom = 1 + (sin (time/130 + findex*3)/64)
    in (x * zoom , y * zoom)