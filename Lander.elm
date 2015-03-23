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
        ship = {x = 0, y = 0, vx =0, vy = 0.1, r=0},
        planet = {x = -150, y = 0, r = 70}
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

evaluate planet ship dt deriv =
    let ship' = {ship | x <- ship.x + deriv.dx * dt,
                        y <- ship.y + deriv.dy * dt,
                        vx <- ship.vx + deriv.dvx * dt,
                        vy <- ship.vy + deriv.dvy * dt}
        (ax,ay) = accel planet ship
    in {dx = ship.vx, dy = ship.vy, dvx = ax, dvy = ay}

accel planet ship =
    let sign x = if x < 0 then -1 else if x > 0 then 1 else 0
        xx = (planet.x-ship.x)
        yy = (planet.y-ship.y)
        d2 = xx^2 + yy^2
        ax = 0.7 * (sign xx) / d2
        ay = 0.7 * (sign yy) / d2
    in (ax, ay)

updateTick dt world =
    let s = world.ship
        rk1 = evaluate world.planet s 0 {dx = 0, dy = 0, dvx = 0, dvy = 0}
        rk2 = evaluate world.planet s (dt/2) rk1
        rk3 = evaluate world.planet s (dt/2) rk2
        rk4 = evaluate world.planet s dt rk3
        dxdt = (rk1.dx + 2*(rk2.dx + rk3.dx) + rk4.dx)/6
        dydt = (rk1.dy + 2*(rk2.dy + rk3.dy) + rk4.dy)/6
        dvxdt = (rk1.dvx + 2*(rk2.dvx + rk3.dvx) + rk4.dvx)/6
        dvydt = (rk1.dvy + 2*(rk2.dvy + rk3.dvy) + rk4.dvy)/6
        ship' = {s | x <- s.x + dxdt*dt, y <- s.y + dydt*dt, vx <- s.vx + dvxdt*dt, vy <- s.vy + dvydt*dt}
    in {world | ship <- ship'}

updateMove arrows world = 
    let s = world.ship
        s' = {s | r <- s.r-(toFloat arrows.x)*5,
                  vx <- s.vx - (toFloat arrows.y) * sin (degrees s.r) * 0.001,
                  vy <- s.vy + (toFloat arrows.y) * cos (degrees s.r) * 0.001}
    in {world | ship <- s'}

-- Display

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
    in collage world.view.w world.view.h [
        filled black (rect w' h'),
        Debug.trace "ship" (
            move (world.ship.x, world.ship.y) <| rotate (degrees world.ship.r) <|
                group [rotate (degrees -30) (outlined (solid white) (ngon 3 25)), outlined (solid white) (rect 3 25)]
            ),
        move (world.planet.x, world.planet.y) <| outlined (solid white) (circle world.planet.r)]

-- Signals

window = Signal.map Viewport Window.dimensions
ticks = Signal.map Tick (fps 30)
keys = Signal.sampleOn (fps 30) (Signal.map Move Keyboard.arrows)

inputs = Signal.mergeMany [window, ticks, keys]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states