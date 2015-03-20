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
        ship = {x = 0, y = 0, vx =0, vy = 0.1},
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
        sign x = if x < 0 then -1 else if x > 0 then 1 else 0
        xx = (world.planet.x-s.x)
        yy = (world.planet.y-s.y)
        d2 = xx^2 + yy^2
        s' = { s |
                x <- s.x + dt * s.vx,
                y <- s.y + dt * s.vy,
                vx <- s.vx + dt * 0.7 * (sign xx) / d2,
                vy <- s.vy + dt * 0.7 * (sign yy) / d2
        }
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