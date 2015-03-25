import Signal
import Signal.Extra
import Graphics.Element (..)
import Graphics.Collage (..)
import Transform2D (..)
import Color (..)
import Time (..)
import Debug
import Window
import Keyboard
-- App imports
import Generic (..)

-- Model

start viewport = case viewport of
    Viewport (w,h) -> {
        view = {w = w, h = h},
        ship = {x = 0, y = 0, vx =0, vy = 0.07, heading=0, m=0.001},
        planet = {x = -150, y = 0, vx =0, vy = 0, r = 70, m=1}
    }

-- Update

type Update = Viewport (Int, Int) | Tick Float | Move {x:Int,y:Int}

update s world = case s of
    Viewport dims -> updateViewport dims world
    Tick dt -> let n = 8 in iterate (updateTick (dt/n)) n world
    Move arrows -> updateMove arrows world

updateViewport (w,h) world =
    let wv = world.view
        v' = { wv | w <- 0, h <- 0}
    in {world | view <- v'}

integrate dt object1 object2 =
    let xx = (object1.x-object2.x)
        yy = (object1.y-object2.y)
        d2 = xx^2 + yy^2
        ax = object1.m * xx / (sqrt(d2) * d2)
        ay = object1.m * yy / (sqrt(d2) * d2)
    in { object2 |
            x <- object2.x + dt * object2.vx + (ax * dt*dt),
            y <- object2.y + dt * object2.vy + (ay * dt*dt),
            vx <- object2.vx + dt * ax,
            vy <- object2.vy + dt * ay
        }

updateTick dt world =
    let s = world.ship
        p = world.planet
    in { world | ship <- integrate dt p s, planet <- integrate dt s p}

updateMove arrows world = 
    let s = world.ship
        s' = {s | heading <- s.heading-(toFloat arrows.x)*5,
                  vx <- s.vx - (toFloat arrows.y) * sin (degrees s.heading) * 0.001,
                  vy <- s.vy + (toFloat arrows.y) * cos (degrees s.heading) * 0.001}
    in {world | ship <- s'}

-- Display

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
    in collage world.view.w world.view.h [
        filled black (rect w' h'),
        Debug.trace "ship" (
            move (world.ship.x, world.ship.y) <| rotate (degrees world.ship.heading) <|
                group [rotate (degrees -30) (outlined (solid white) (ngon 3 25)), outlined (solid white) (rect 3 25)]
            ),
        Debug.trace "planet" (
            move (world.planet.x, world.planet.y) <| outlined (solid white) (circle world.planet.r))
        ]
        

-- Signals

window = Signal.map Viewport Window.dimensions
ticks = Signal.map Tick (fps 30)
keys = Signal.sampleOn (fps 30) (Signal.map Move Keyboard.arrows)

inputs = Signal.mergeMany [window, ticks, keys]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states