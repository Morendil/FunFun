import Signal
import Signal.Extra
import Graphics.Element (..)
import Graphics.Collage (..)
import Transform2D
import Color (..)
import Time (..)
import List (..)
import Debug
import Window
import Keyboard
-- App imports
import Generic (..)

-- Model

type alias Mass = {x:Float,y:Float,vx:Float,vy:Float,m:Float}
type Body = Ship {heading: Float} | Planet {r:Float}

start viewport = case viewport of
    Viewport (w,h) -> {
        view = {w = w, h = h},
        zoom = 1.0,
        time = 0,
        bodies = [{mass = {x = 0, y = 0, vx =0, vy = 0.06, m=0.001}, body = Ship {heading=0}},
                  {mass = {x = -150, y = 0, vx =0, vy = 0, m=1},     body = Planet {r = 70}},
                  {mass = {x = -80, y = 80, vx =0.05, vy = -0.05, m=0.01},   body = Planet {r = 7}}]
    }

-- Update

type Update = Viewport (Int, Int) | Tick Float | Move {x:Int,y:Int}

update s world = case s of
    Viewport dims -> updateViewport dims world
    Tick dt -> let n = 8 in iterate (updateTick (dt/n)) n world
    Move arrows -> updateMove arrows world

updateViewport (w,h) world =
    let wv = world.view
        v' = { wv | w <- w, h <- h}
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
    let integrateAll dt bodies one = foldl (integrate dt) one bodies
        updateMass e m = {e | mass <- m}
        bodies = mapAllBut (integrateAll dt) (map .mass world.bodies)
        zoom' = 1 - ((sin (world.time / 1000)) / 20)
    in { world | bodies <- map2 updateMass world.bodies bodies, zoom <- zoom', time <- world.time + dt}

updateMove arrows world = 
    let (s :: rest) = world.bodies
        (Ship body) = s.body
        smass = s.mass
        body' = {body | heading <- body.heading-(toFloat arrows.x)*5}
        mass' = {smass | vx <- s.mass.vx - (toFloat arrows.y) * sin (degrees body.heading) * 0.001,
                        vy <- s.mass.vy + (toFloat arrows.y) * cos (degrees body.heading) * 0.001}
        s' = {s | body <- Ship body',
                  mass <- mass'}
    in {world | bodies <- s' :: rest}

-- Display

displayBody {mass,body} =
    case body of
        Ship {heading} -> move (mass.x, mass.y) <| rotate (degrees heading)
                                                <| group [rotate (degrees -30) (outlined (solid white) (ngon 3 25)), outlined (solid white) (rect 3 25)]
        Planet {r} -> move (mass.x, mass.y) <| outlined (solid white) (circle r)

display world =
    let (w',h') = (toFloat world.view.w, toFloat world.view.h)
        sky = [filled black (rect w' h')]
        bodies = sky ++ [groupTransform (Transform2D.scale world.zoom) (map displayBody world.bodies)]
    in collage world.view.w world.view.h bodies

-- Signals

window = Signal.map Viewport Window.dimensions
ticks = Signal.map Tick (fps 30)
keys = Signal.sampleOn (fps 30) (Signal.map Move Keyboard.arrows)

inputs = Signal.mergeMany [window, ticks, keys]

main =
    let states = Signal.Extra.foldp' update start inputs
    in Signal.map display states