import Signal
import Signal.Extra
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Debug
import Window

-- Model

start viewport = case viewport of
    Viewport (w,h) -> {
        view = {w = w, h = h},
        ship = {x = 0, y = 0, vx =0, vy = 0}
    }

-- Update

type Update = Viewport (Int, Int)

update s world = case s of
    Viewport dims -> updateViewport dims world

updateViewport (w,h) world =
    let wv = world.view
        v' = { wv | w <- 0, h <- 0}
    in {world | view <- v'}

-- Display

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
    in collage world.view.w world.view.h [
        filled black (rect w' h'),
        rotate (degrees 90) <| outlined (solid white) (ngon 3 30)]

-- Signals

inputs = Signal.map Viewport Window.dimensions

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states