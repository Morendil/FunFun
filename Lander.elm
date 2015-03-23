import Signal
import Signal.Extra
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Time (..)
import Debug
import Window
-- App imports
import Generic (..)

-- Model

start viewport = case viewport of
    Viewport (w,h) -> {
        view = {w = w, h = h},
        ship = {x = 0, y = 0, vx =0, vy = 0.09},
        planet = {x = -150, y = 0, r = 70}
    }

-- Update

type Update = Viewport (Int, Int) | Tick Float

update s world = case s of
    Viewport dims -> updateViewport dims world
    Tick dt -> let n = 8 in iterate (updateTick (dt/n)) n world

updateViewport (w,h) world =
    let wv = world.view
        v' = { wv | w <- 0, h <- 0}
    in {world | view <- v'}

updateTick dt world =
    let s = world.ship
        moins (x1,y1) (x2,y2) = (x1-x2,y1-y2)
        fois f (x1,y1) = (f*x1,f*y1)
        center object = (object.x,object.y)
        distance (x1,y1) (x2,y2) = sqrt((x2-x1)^2+(y2-y1)^2)
        r = distance (center s) (center world.planet)
        f = 1/r^2
        (ax,ay) = f `fois` ((1/r) `fois` ((center world.planet) `moins` (center s)))
        (vx,vy) = (ax*dt+s.vx,ay*dt+s.vy)
        (x,y) = (vx*dt+s.x,vy*dt+s.y)
        --
        s' = {s | x <- x, y <- y, vx <- vx, vy <- vy}
    in {world | ship <- s'}

-- Display

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
    in collage world.view.w world.view.h [
        filled black (rect w' h'),
        Debug.trace "ship" (move (world.ship.x, world.ship.y) <| rotate (degrees 90) <| outlined (solid white) (ngon 3 30)),
        move (world.planet.x, world.planet.y) <| outlined (solid white) (circle world.planet.r)]

-- Signals

window = Signal.map Viewport Window.dimensions
ticks = Signal.map Tick (fps 30)

inputs = Signal.mergeMany [window, ticks]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states